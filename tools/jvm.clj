#!/usr/bin/env bb
(require '[babashka.fs :as fs]
         '[babashka.http-client :as http]
         '[babashka.process :refer [shell]]
         '[cheshire.core :as json]
         '[clojure.string :as string]) 

(def ^:private FORCE (Boolean/parseBoolean (System/getenv "DOTFILES_TOOLS_FORCE")))
(def ^:private xdg-config-home (fs/expand-home (System/getenv "XDG_CONFIG_HOME")))
(def ^:private jvm-root-dir (fs/path "/usr/lib/jvm"))

(defn- download-file
  [link path]
  (shell "wget" "-O" path link))

(defn- parse-jdk-version
  [version-str]
  (let [nilify (fn [s] (when-not (string/blank? s) s))
        int (fn [s] (when s (Integer/parseInt s)))
        sanitized (string/replace (or version-str "") #"\-beta" "")
        [version meta] (string/split (or sanitized "") #"\+")
        [major minor patch] (string/split (or version "") #"\.")
        [build optional] (string/split (or meta "") #"\-")]
    {:major (-> major nilify int)
     :minor (-> minor nilify int)
     :patch (-> patch nilify int)
     :build (-> build nilify int)
     :optional (nilify optional)}))

(defn- jdk-version-str
  [version-data]
  (str (:major version-data)
       (when-let [minor (:minor version-data)] (str "." minor))
       (when-let [patch (:patch version-data)] (str "." patch))
       (when-let [build (:build version-data)] (str "+" build))
       (when-let [optional (:optional version-data)] (str "-" optional))))

(defn- make-temp-file
  []
  (fs/path (string/trim (:out (shell {:out :string} "mktemp")))))

(defn- make-temp-directory
  []
  (fs/path (string/trim (:out (shell {:out :string} "mktemp" "-d")))))

(defn- checksum-valid?
  [file-path checksum]
  ; do it this way so we can mangle the file name)
  (-> (shell {:out :string} "sha256sum" file-path)
      :out
      string/trim
      (string/split #" ")
      first
      (= checksum)))

(defn- jdk-dir
  [version]
  (fs/path jvm-root-dir version))

(defn- list-jdk-releases
  []
  (let [versions (-> (http/get "https://api.adoptium.net/v3/info/available_releases"
                               {:headers {"Accept" "application/json"}})
                     :body
                     (json/parse-string true))]
    {:lts (:most_recent_lts versions)
     :latest (:tip_version versions)}))

(defn- jdk-assets-metadata
  [version lts?]
  (let [release (-> (http/get (str "https://api.adoptium.net/v3/assets/feature_releases/" version "/" (if lts? "ga" "ea") "?architecture=x64&heap_size=normal&image_type=jdk&jvm_impl=hotspot&page=0&page_size=10&project=jdk&sort_method=DEFAULT&sort_order=DESC&vendor=eclipse" "&os=" "linux")
                              {:headers {"Accept" "application/json"}})
                    :body
                    (json/parse-string true)
                    first)
        version (:version_data release)
        version-data {:major (:major version)
                      :minor (:minor version)
                      :patch (:security version)
                      :build (:build version)
                      :optional (:optional version)}
        package (-> release
                    :binaries
                    first
                    :package)]
    {:version (jdk-version-str version-data)
     :version-data version-data
     :link (:link package)
     :checksum (:checksum package)
     :checksum-link (:checksum_link package)
     :signature-link (:signature_link package)}))

(defn- jenv-versions
  []
  (->> (shell {:out :string} "jenv" "versions")
       :out
       (string/split-lines)
       (map string/trim)
       (map (fn [v]
              (if (string/starts-with? v "*")
                (let [splits (string/split v #" ")]
                  {:version (nth splits 1)
                   :default true})
                {:version v})))))

(defn- ensure-jvm-root-dir
  []
  (when-not (fs/directory? jvm-root-dir)
    (fs/create-dir jvm-root-dir)))

(defn- jvm-dir-versions
  []
  (->> (fs/list-dir jvm-root-dir)
       (map #(.getFileName %))
       (map str)
       (mapv parse-jdk-version)))

(defn- download-jdk
  [metadata]
  (let [temp-file (make-temp-file)]
    (download-file (:link metadata) temp-file)
    (if-not (checksum-valid? temp-file (:checksum metadata))
      (throw (Exception. "JDK checksum invalid!"))
      temp-file)))

(defn- unpack-jdk
  [version file-path]
  (shell "sudo" "mkdir" (jdk-dir version))
  (shell "sudo" "tar" "-xvzf" file-path "-C" (jdk-dir version) "--strip-components" "1"))

(defn- install-jdk
  [major-version lts? & {:keys [force?] :or {force? FORCE}}]
  (ensure-jvm-root-dir)
  (let [major-version (if (string? major-version) (Integer/parseInt major-version) major-version)
        metadata (jdk-assets-metadata major-version lts?)
        installed-versions (jvm-dir-versions)
        already-installed? (some (fn [ver] (= (:version metadata) ver)) installed-versions)]
    (when (or (not already-installed?) force?)
      (last
       (doall
         [(shell "sudo" "rm" "-rf" (jdk-dir (:version metadata)))
          (->> (download-jdk metadata)
               (unpack-jdk (:version metadata)))
          (:version metadata)])))))
    
(defn- install-jenv
  ([]
   (install-jenv {}))
  ([{:keys [force? jenv-home] :or {force? FORCE jenv-home (fs/path xdg-config-home "jenv")}}]
   (cond
     (not (fs/directory? jenv-home))
     (shell "git" "clone" "-q" "--depth" "1" "https://github.com/jenv/jenv.git" jenv-home)

     force?
     (do
       (shell "git" "-C" jenv-home "fetch" "-q" "--depth" "1" "origin" "master")
       (shell "git" "-C" jenv-home "reset" "-q" "--hard" "origin/master")))))

(defn- register-jdk
  [version]
  (shell "jenv" "add" (jdk-dir version))
  (shell "jenv" "rehash"))

(defn- unregister-old-jdks-for-major
  [version]
  (let [version-data (parse-jdk-version version)
        jenv-versions (jenv-versions)
        temurin-majors (filter #(string/starts-with? (:version %) (str "temurin64-" (:major version-data))) jenv-versions)
        majors (filter #(string/starts-with? (:version %) (str (:major version-data))) jenv-versions)]
    (doseq [version (map :version (concat temurin-majors majors))] (shell "jenv" "remove" version))))

(defn- install-with-jenv
  [major-version lts?]
  (let [version (install-jdk major-version lts?)]
    (when version
      (unregister-old-jdks-for-major version)
      (register-jdk version)
      version)))

(defn- set-default-global-version
  [major-version]
  (shell "jenv" "global" major-version))

(defn- install-jdks
  []
  (let [versions (list-jdk-releases)]
    (doall
     [(install-with-jenv (:lts versions) true)
      (install-with-jenv (:latest versions) false)
      (set-default-global-version (:lts versions))])))

(install-jenv)
(install-jdks)

