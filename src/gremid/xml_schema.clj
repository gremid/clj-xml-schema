(ns gremid.xml-schema
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   (com.thaiopensource.relaxng.translate Driver)
   (java.io File InputStream OutputStream Reader StringReader StringWriter Writer)
   (java.net URI URL)
   (javax.xml XMLConstants)
   (javax.xml.transform Source URIResolver)
   (javax.xml.transform.dom DOMSource)
   (javax.xml.transform.stream StreamSource)
   (javax.xml.validation Schema SchemaFactory)
   (net.sf.saxon Configuration)
   (net.sf.saxon.s9api DocumentBuilder Processor Serializer XdmDestination XdmNode XdmValue XPathCompiler XPathExecutable XsltCompiler XsltExecutable)
   (org.w3c.dom Node NodeList)
   (org.w3c.dom.ls LSInput LSResourceResolver)
   (org.xml.sax ErrorHandler InputSource SAXParseException)))

(defn resolve-uri
  "Resolves URIs, with support for the jar URL scheme."
  ^URI [^URI base ^URI uri]
  (if (= "jar" (.. base (getScheme)))
    (let [[base-jar base-path] (str/split (str base) #"!")
          resolved             (.. (URI. base-path) (resolve uri))]
      (if-not (.isAbsolute resolved) (URI. (str base-jar "!" resolved)) resolved))
    (.resolve base uri)))

(def resolver
  "A resolver with support for resources from JARs on the classpath"
  (proxy [URIResolver LSResourceResolver] []
    (resolve [^String href ^String base]
      (let [base (URI. (or (not-empty base) ""))
            href (URI. (or (not-empty href) ""))]
        (StreamSource. (str (resolve-uri base href)))))
    (resolveResource [_ _ _ ^String href ^String base]
      (when href
        (let [base (URI. (or (not-empty base) ""))
              href (URI. (or (not-empty href) ""))
              uri  (resolve-uri base href)]
          (proxy [LSInput] []
            (getSystemId [] (str uri))
            (getByteStream [] (io/input-stream uri))
            (getEncoding [])
            (getStringData [])
            (getCharacterStream [])
            (getPublicId [])
            (getBaseURI [])))))))

(def ^Configuration configuration
  (doto (Configuration.)
    (.setURIResolver ^URIResolver resolver)))

(def ^Processor processor
  (Processor. configuration))

(def ^DocumentBuilder doc-builder
  (doto (.newDocumentBuilder processor)
    (.setLineNumbering true)))

(def ^XsltCompiler xslt-compiler
  (.newXsltCompiler processor))

(defn xpath-compiler
  ^XPathCompiler  [ns-decls]
  (let [xpath-compiler (.newXPathCompiler processor)]
    (doseq [[prefix uri] ns-decls]
      (.declareNamespace xpath-compiler prefix uri))
    xpath-compiler))

(defprotocol IO
  (->source [v])
  (->xdm [v])
  (->serializer [v]))

(extend-protocol IO
  Source
  (->source [^Source v] v)
  (->xdm [v] (.build doc-builder v))

  NodeList
  (->xdm [v] (.wrap doc-builder v))

  Node
  (->source [^Node v] (DOMSource. v))
  (->xdm [v] (.wrap doc-builder v))

  XdmNode
  (->source [v] (.asSource v))
  (->xdm [v] v)

  XdmDestination
  (->serializer [v] v)

  XdmValue
  (->xdm [v] v)

  Serializer
  (->serializer [v] v)

  File
  (->source [^File v] (StreamSource. v))
  (->xdm [v] (.build doc-builder v))
  (->serializer [v] (.newSerializer processor v))

  URI
  (->source [^URI v] (StreamSource. (str v)))
  (->input-source [^URI v] (InputSource. (str v)))
  (->xdm [v] (->xdm (->source v)))

  URL
  (->source [^URL v] (->source (.toURI v)))
  (->xdm [v] (->xdm (->source v)))

  InputStream
  (->source [^InputStream v] (StreamSource. v))
  (->xdm [v] (->xdm (->source v)))

  OutputStream
  (->serializer [v] (.newSerializer processor v))

  Reader
  (->source [^Reader v] (StreamSource. v))
  (->xdm [v] (->xdm (->source v)))

  Writer
  (->serializer [v] (.newSerializer processor v))

  String
  (->source [^String v] (->source (StringReader. v)))
  (->xdm [v] (->xdm (->source v)))
  (->serializer [v] (->serializer (io/file v))))

(defn ->xslt
  ^XsltExecutable [stylesheet]
  (.compile xslt-compiler (->source stylesheet)))

(defn  ->xpath
  ^XPathExecutable [^XPathCompiler xpath-compiler ^String s]
  (.compile xpath-compiler s))

(defn serialize
  ([source destination]
   (.serializeNode ^Serializer (->serializer destination) (->xdm source)))
  ([source]
   (let [writer (StringWriter.)]
     (serialize source writer)
     (str writer))))

(defn transform
  ([^XsltExecutable stylesheet source]
   (let [destination (XdmDestination.)]
     (transform stylesheet source destination)
     (.getXdmNode destination)))
  ([^XsltExecutable stylesheet source destination]
   (let [source      (->source source)
         destination (->serializer destination)]
     (.. stylesheet (load30) (transform source destination)))))

(defn select ^XdmValue
  [^XPathExecutable xp ctx]
  (.. (doto (.load xp) (.setContextItem (->xdm ctx))) (evaluate)))

(defn selector
  [xpath-compiler ^String s]
  (partial select (->xpath xpath-compiler s)))

(defn rnc->rng
  [rnc rng]
  (when-not (= 0 (.run (Driver.) (into-array String (list rnc rng))))
    (throw (ex-info "Error while converting RNC to RNG" {:rnc rnc :rng rng}))))

(def rng-schema-factory
  (let [^SchemaFactory sf
        (try
          (SchemaFactory/newInstance XMLConstants/RELAXNG_NS_URI)
          (catch IllegalArgumentException _
            (System/setProperty
             (str (.getName SchemaFactory) ":" XMLConstants/RELAXNG_NS_URI)
             "com.thaiopensource.relaxng.jaxp.XMLSyntaxSchemaFactory")
            (SchemaFactory/newInstance XMLConstants/RELAXNG_NS_URI)))]
    (doto sf (.setResourceResolver resolver))))

(defn ->rng-schema
  [rng]
  (.newSchema ^SchemaFactory rng-schema-factory ^Source (->source rng)))

(defn ->rng-error
  "Convert a RELAX NG error to an error record map."
  [severity ^SAXParseException e]
  {:severity severity
   :line     (.getLineNumber e)
   :column   (.getColumnNumber e)
   :message  (.getMessage e)})

(defn rng-validate
  [^Schema schema source]
  (let [validator     (.newValidator schema)
        errors        (transient [])
        add-error     #(conj! errors (->rng-error %1 %2))
        error-handler (proxy [ErrorHandler] []
                        (error [e] (add-error :error e))
                        (fatalError [e] (add-error :fatal e))
                        (warning [e] (add-error :warning e)))]
    (.setErrorHandler validator error-handler)
    (.validate validator (->source source))
    (persistent! errors)))

(defn resource->xslt
  "Compile XSLT stylesheet from classpath resource."
  [r]
  (->xslt (.toURI (io/resource r))))

(def rng->sch
  "Extract Schematron rules from RELAX NG stylesheet."
  (resource->xslt "ExtractSchFromRNG-2.xsl"))

(def sch->sch-xslt
  "Compile Schematron rules into XSLT stylesheet."
  (resource->xslt "iso_svrl_for_xslt2.xsl"))

(defn rng->sch-xsl
  "Derives a validating Schematron XSLT from a RELAX NG schema with embedded rules."
  [rng-source xsl-destination]
  (let [^File sch (File/createTempFile "gremid.data.xml.schematron." ".sch")]
    (try
      (transform rng->sch rng-source sch)
      (transform sch->sch-xslt sch xsl-destination)
      (finally
        (.delete sch)))))

(def sch-xpath-compiler
  (xpath-compiler {"svrl" "http://purl.oclc.org/dsdl/svrl"}))

(def sch-failures
  "Select Schematron failures."
  (selector sch-xpath-compiler ".//svrl:failed-assert"))

(defn sch-xp-str
  "Creates a string-extracting function based on a XPath."
  [xp]
  (comp str/join (selector sch-xpath-compiler xp)))

(def sch-failure-loc
  "Select Schematron failure locations."
  (sch-xp-str "string(@location)"))

(def sch-failure-text
  "Select Schematron failure messages."
  (sch-xp-str "svrl:text/text()"))

(defn ->sch-error
  "Convert a Schematron failure to an error record."
  [doc failure]
  (let [location      (sch-failure-loc failure)
        selector      (selector xpath-compiler location)
        ^XdmNode node (-> doc selector first)]
    {:line    (.getLineNumber node)
     :column  (.getColumnNumber node)
     :message (sch-failure-text failure)}))

(defn sch-validate
  [sch-xslt source]
  (let [doc        (->xdm source)
        sch-report (transform sch-xslt doc)]
    (into [] (map (partial ->sch-error doc)) (sch-failures sch-report))))
