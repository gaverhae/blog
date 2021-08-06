(ns cryogen.compiler
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [clojure.zip :as zip]
            [io.aviso.exception :refer [write-exception]]
            [net.cgrand.enlive-html :as enlive]
            [schema.core :as s]
            [selmer.parser :refer [cache-off!]]
            [text-decoration.core :refer :all]
            [cryogen-core.io :as cryogen-io]
            [cryogen-core.config :refer [resolve-config]]
            [cryogen-core.klipse :as klipse]
            [cryogen-core.markup :as m]
            [cryogen-core.rss :as rss]
            [cryogen-core.sass :as sass]
            [cryogen-core.schemas :as schemas]
            [cryogen-core.sitemap :as sitemap]
            [cryogen-core.util :as util]
            [cryogen-core.zip-util :as zip-util]
            [cryogen-core.toc :as toc]
            [clojure.string :as str])
  (:import java.util.Locale
           (java.util Date)
           (java.net URLEncoder)))

(cache-off!)

(def content-root "content")

(defn only-changed-files-filter
  "Returns a page/post filter that only accepts these files:

  - no filtering if `changeset` is empty (as it means this is the first build)
  - recompile the post/page that has changed since the last compilation
  - recompile everything if a template HTML file has changed

  Arguments:
  - `changeset` - sequence of `java.io.File` of relative paths within the
    project, such as `File[content/asc/pages/about.asc]`, as provided by
    [[cryogen-core.watcher/find-changes]]

  Downsides: Tags, archives, index etc. will only know about the recompiled
  post and not the others. Previous/next links on neighbour posts won't be
  updated either."
  [changeset]
  (let [changed-paths      (->> changeset (map #(.getPath %)) set)
        theme-html-change? (some
                             (fn [^java.io.File file]
                               (and (string/starts-with?
                                      (.getPath file) "themes/")
                                    (string/ends-with?
                                      (.getName file) ".html")))
                             changeset)]
    (if (empty? changeset)
      (constantly true)
      (fn [^java.io.File page]
        (or
          theme-html-change?
          (contains? changed-paths (.getPath page)))))))

(defn re-pattern-from-exts
  "Creates a properly quoted regex pattern for the given file extensions"
  [exts]
  (re-pattern (str "(" (string/join "|" (map #(string/replace % "." "\\.") exts)) ")$")))

(defn find-entries
  "Returns a list of files under the content directory according to the
  implemented Markup protocol and specified root directory. It defaults to
  looking under the implemented protocol's subdirectory, but fallsback to look
  at the content directory."
  [root mu ignored-files]
  (let [assets (cryogen-io/find-assets
                 (cryogen-io/path content-root (m/dir mu) root)
                 (m/exts mu)
                 ignored-files)]
    (if (seq assets)
      assets
      (cryogen-io/find-assets
        (cryogen-io/path content-root root)
        (m/exts mu)
        ignored-files))))

(defn parse-post-date
  "Parses the post date from the post's file name and returns the corresponding java date object"
  [^String file-name date-fmt]
  (let [fmt (java.text.SimpleDateFormat. date-fmt)]
    (if-let [last-slash (string/last-index-of file-name "/")]
      (.parse fmt (.substring file-name (inc last-slash) (+ last-slash 11)))
      (.parse fmt (.substring file-name 0 10)))))


(defn page-uri
  "Creates a URI from file name. `uri-type` is any of the uri types specified in config, e.g., `:post-root-uri`."
  ([file-name params]
   (page-uri file-name nil params))
  ([file-name uri-type {:keys [blog-prefix clean-urls] :as params}]
   (let [page-uri (get params uri-type)
         uri-end  (condp = clean-urls
                    :trailing-slash (string/replace file-name #"(index)?\.html" "/")
                    :no-trailing-slash (string/replace file-name #"(index)?\.html" "")
                    :dirty file-name)]
     (cryogen-io/path "/" blog-prefix page-uri uri-end))))

(defn read-page-meta
  "Returns the clojure map from the top of a markdown page/post"
  [page rdr]
  (try
    (let [metadata (read rdr)]
      (s/validate schemas/MetaData metadata)
      metadata)
    (catch Exception e
      (throw (ex-info (ex-message e)
                      (assoc (ex-data e) :page page))))))

(defn page-content
  "Returns a map with the given page's file-name, metadata and content parsed from
  the file with the given markup."
  [^java.io.File page config markup]
  (with-open [rdr (java.io.PushbackReader. (io/reader page))]
    (let [re-root     (re-pattern (str "^.*?(" (:page-root config) "|" (:post-root config) ")/"))
          page-fwd    (string/replace (str page) "\\" "/")  ;; make it work on Windows
          page-name   (if (:collapse-subdirs? config) (.getName page) (string/replace page-fwd re-root ""))
          file-name   (string/replace page-name (re-pattern-from-exts (m/exts markup)) ".html")
          page-meta   (read-page-meta page-name rdr)
          content     ((m/render-fn markup) rdr (assoc config :page-meta page-meta))
          content-dom (util/trimmed-html-snippet content)]
      {:file-name   file-name
       :page-meta   page-meta
       :content-dom content-dom})))

(defn add-toc
  "Adds :toc to article, if necessary"
  [{:keys [content-dom toc toc-class] :as article} config]
  (update
    article
    :toc
    #(if %
       (toc/generate-toc content-dom
                         {:list-type toc
                          :toc-class (or toc-class (:toc-class config) "toc")}))))

(defn merge-meta-and-content
  "Merges the page metadata and content maps"
  [file-name page-meta content-dom]
  (merge
    (update-in page-meta [:layout] #(str (name %) ".html"))
    {:file-name   file-name
     :content-dom content-dom}))

(defn parse-page
  "Parses a page/post and returns a map of the content, uri, date etc."
  [page config markup]
  (let [{:keys [file-name page-meta content-dom]} (page-content page config markup)]
    (-> (merge-meta-and-content file-name (update page-meta :layout #(or % :page)) content-dom)
        (merge
          {:type          :page
           :uri           (page-uri file-name :page-root-uri config)
           :page-index    (:page-index page-meta)
           :klipse/global (:klipse config)
           :klipse/local  (:klipse page-meta)})
        (add-toc config))))

(defn parse-post
  "Return a map with the given post's information."
  [page config markup]
  (let [{:keys [file-name page-meta content-dom]} (page-content page config markup)]
    (let [date            (if (:date page-meta)
                            (.parse (java.text.SimpleDateFormat. (:post-date-format config)) (:date page-meta))
                            (parse-post-date file-name (:post-date-format config)))
          archive-fmt     (java.text.SimpleDateFormat. ^String (:archive-group-format config) (Locale/getDefault))
          formatted-group (.format archive-fmt date)]
      (-> (merge-meta-and-content file-name (update page-meta :layout #(or % :post)) content-dom)
          (merge
            {:type                    :post
             :date                    date
             :formatted-archive-group formatted-group
             :parsed-archive-group    (.parse archive-fmt formatted-group)
             :uri                     (page-uri file-name :post-root-uri config)
             :tags                    (set (:tags page-meta))
             :klipse/global           (:klipse config)
             :klipse/local            (:klipse page-meta)})
          (add-toc config)))))

(defn read-posts
  "Returns a sequence of maps representing the data from markdown files of posts.
   Sorts the sequence by post date."
  [config incremental-compile-filter]
  (->> (m/markups)
       (mapcat
         (fn [mu]
           (->>
             (find-entries (:post-root config)
                           mu
                           (:ignored-files config))
             (filter incremental-compile-filter)
             (pmap #(parse-post % config mu))
             (remove #(= (:draft? %) true)))))
       (sort-by :date)
       reverse
       (drop-while #(and (:hide-future-posts? config) (.after ^Date (:date %) (Date.))))))

(defn read-pages
  "Returns a sequence of maps representing the data from markdown files of pages.
  Sorts the sequence by post date."
  [config incremental-compile-filter]
  (->> (m/markups)
       (mapcat
         (fn [mu]
           (->>
             (find-entries (:page-root config)
                           mu
                           (:ignored-files config))
             (filter incremental-compile-filter)
             (map #(parse-page % config mu)))))
       (sort-by :page-index)))

(defn tag-post
  "Adds the uri and title of a post to the list of posts under each of its tags"
  [tags post]
  (reduce (fn [tags tag]
            (update-in tags [tag] (fnil conj []) (select-keys post [:uri :title :content-dom :date :enclosure :description])))
          tags
          (:tags post)))

(defn group-by-tags
  "Maps all the tags with a list of posts that contain each tag"
  [posts]
  (reduce tag-post {} posts))

(defn group-for-archive
  "Groups the posts by month and year for archive sorting"
  [posts]
  (->> posts
       (map #(select-keys % [:title :uri :date :formatted-archive-group :parsed-archive-group]))
       (group-by :formatted-archive-group)
       (map (fn [[group posts]]
              {:group        group
               :parsed-group (:parsed-archive-group (get posts 0))
               :posts        (map #(select-keys % [:title :uri :date]) posts)}))
       (sort-by :parsed-group)
       reverse))

(defn group-for-author
  "Groups the posts by author. If no post author if found defaults `default-author`."
  [posts default-author]
  (->> posts
       (map #(select-keys % [:title :uri :date :formatted-archive-group :parsed-archive-group :author]))
       (map #(update % :author (fn [author] (or author default-author))))
       (group-by :author)
       (map (fn [[author posts]]
              {:author author
               :posts  posts}))))

(defn url-encode
  "Url encode path element. Encodes spaces as %20 instead of +,
  because some webservers pass + through to the file system"
  [str]
  (-> str
      (URLEncoder/encode "UTF-8")
      (str/replace "+" "%20")))

(defn tag-info
  "Returns a map containing the name and uri of the specified tag"
  [config tag]
  {:name (name tag)
   :file-path (page-uri (str (name tag) ".html") :tag-root-uri config)
   :uri  (page-uri (str (url-encode (name tag)) ".html") :tag-root-uri config)})

(defn add-prev-next
  "Adds a :prev and :next key to the page/post data containing the metadata of the prev/next
  post/page if it exists"
  [pages]
  (map (fn [[prev target next]]
         (assoc target
           :prev (if prev (dissoc prev :content-dom) nil)
           :next (if next (dissoc next :content-dom) nil)))
       (partition 3 1 (flatten [nil pages nil]))))

(defn group-pages
  "Separates the pages into links for the navbar and links for the sidebar"
  [pages]
  (let [{navbar-pages  true
         sidebar-pages false} (group-by #(boolean (:navbar? %)) pages)]
    (map (partial sort-by :page-index) [navbar-pages sidebar-pages])))

(defn write-html
  "When `clean-urls` is set to:
  - `:trailing-slash` appends `/index.html`.
  - `:no-trailing-slash` appends `.html`.
  - `:dirty` just spits."
  [file-uri {:keys [blog-prefix clean-urls]} data]
  (condp = clean-urls
    :trailing-slash (cryogen-io/create-file-recursive
                     (cryogen-io/path file-uri "index.html") data)
    :no-trailing-slash (cryogen-io/create-file
                        (if (or (= blog-prefix file-uri) (= "/" file-uri))
                          (cryogen-io/path file-uri "index.html")
                          (str file-uri ".html"))
                        data)
    :dirty (cryogen-io/create-file file-uri data)))

(defn- print-debug-info [data]
  (println "DEBUG:")
  (pprint data))

(defn content-dom->html [{dom :content-dom :as article}]
  (-> article
      (dissoc :content-dom)
      (assoc :content (util/enlive->html-text dom))))

(defn htmlize-content [{:keys [postprocess-article-html-fn] :as params}]
  (letfn [(postprocess-article [article]
            (if postprocess-article-html-fn
              (postprocess-article-html-fn article params)
              article))
          (htmlize-article [article]
            (-> article
                content-dom->html
                postprocess-article))]
    (cond
      (contains? params :posts) (update params :posts (partial map htmlize-article))
      (contains? params :post) (update params :post htmlize-article)
      (contains? params :page) (update params :page htmlize-article)
      :else params)))

(defn render-file
  "Wrapper around `selmer.parser/render-file` with pre-processing"
  [file-path params]
  (selmer.parser/render-file
    file-path
    (htmlize-content params)))

(defn compile-articles
  [cat-str cat-kw articles root-uri-key {:keys [debug? blog-prefix] :as params}]
  (when-not (empty? articles)
    (println (blue (str "compiling " cat-str)))
    (let [root-uri (root-uri-key params)]
      (cryogen-io/create-folder (cryogen-io/path "/" blog-prefix root-uri))
      (doseq [{:keys [uri] :as article} articles]
        (println "-->" (cyan uri))
        (when debug?
          (print-debug-info article))
        (write-html uri
                    params
                    (render-file (str "/html/" (:layout article))
                                 (merge params
                                        {cat-kw article
                                         :selmer/context (cryogen-io/path "/" blog-prefix "/")
                                         :uri uri})))))))

(defn compile-pages
  "Compiles all the pages into html and spits them out into the public folder"
  [params pages]
  (compile-articles "pages" :page pages :page-root-uri
                    (merge params {:home false
                                   :active-page "pages"})))

(defn compile-posts
  "Compiles all the posts into html and spits them out into the public folder"
  [params posts]
  (compile-articles "posts" :post posts :post-root-uri
                    (merge params {:active-page "posts"})))

(defn compile-tags
  "Compiles all the tag pages into html and spits them out into the public folder"
  [{:keys [blog-prefix tag-root-uri] :as params} posts-by-tag]
  (when-not (empty? posts-by-tag)
    (println (blue "compiling tags"))
    (cryogen-io/create-folder (cryogen-io/path "/" blog-prefix tag-root-uri))
    (doseq [[tag posts] posts-by-tag]
      (let [{:keys [name uri file-path]} (tag-info params tag)]
        (println "-->" (cyan uri))
        (write-html file-path
                    params
                    (render-file "/html/tag.html"
                                 (merge params
                                        {:active-page     "tags"
                                         :selmer/context  (cryogen-io/path "/" blog-prefix "/")
                                         :name            name
                                         :posts           posts
                                         :uri             uri})))))))

(defn compile-tags-page [{:keys [blog-prefix] :as params}]
  "Compiles a page with links to each tag page. Spits the page into the public folder"
  (println (blue "compiling tags page"))
  (let [uri (page-uri "tags.html" params)]
    (write-html uri
                params
                (render-file "/html/tags.html"
                             (merge params
                                    {:active-page     "tags"
                                     :selmer/context  (cryogen-io/path "/" blog-prefix "/")
                                     :uri             uri})))))

(defn content-until-more-marker
  "Returns the content until the <!--more--> special comment,
  closing any unclosed tags. Returns nil if there's no such comment."
  [content-dom]
  (some-> (zip/xml-zip {:content content-dom})
          (zip-util/cut-tree-vertically zip-util/more-marker?)
          (zip/node)
          :content))

(defn preview-dom [blocks-per-preview content-dom]
  (or (content-until-more-marker content-dom)
      (take blocks-per-preview content-dom)))

(defn create-preview
  "Creates a single post preview"
  [blocks-per-preview post]
  (update post :content-dom
          #(preview-dom blocks-per-preview %)))

(defn create-previews
  "Returns a sequence of vectors, each containing a set of post previews"
  [posts posts-per-page blocks-per-preview]
  (->> posts
       (map #(create-preview blocks-per-preview %))
       (partition-all posts-per-page)
       (map-indexed (fn [i v] {:index (inc i) :posts v}))))

(defn create-preview-links
  "Turn each vector of previews into a map with :prev and :next keys that contain the uri of the
  prev/next preview page"
  [previews params]
  (mapv (fn [[prev target next]]
          (merge target
                 {:prev (if prev (page-uri (cryogen-io/path "p" (str (:index prev) ".html")) params) nil)
                  :next (if next (page-uri (cryogen-io/path "p" (str (:index next) ".html")) params) nil)}))
        (partition 3 1 (flatten [nil previews nil]))))

(defn compile-preview-pages
  "Compiles a series of pages containing 'previews' from each post"
  [{:keys [blog-prefix posts-per-page blocks-per-preview] :as params} posts]
  (when-not (empty? posts)
    (let [previews (-> posts
                       (create-previews posts-per-page blocks-per-preview)
                       (create-preview-links params))
          previews (if (> (count previews) 1)
                     (assoc-in previews [1 :prev] (page-uri "index.html" params))
                     previews)]
      (cryogen-io/create-folder (cryogen-io/path "/" blog-prefix "p"))
      (doseq [{:keys [index posts prev next]} previews
              :let [index-page? (= 1 index)]]
        (write-html
          (if index-page? (page-uri "index.html" params)
                          (page-uri (cryogen-io/path "p" (str index ".html")) params))
          params
          (render-file "/html/previews.html"
                       (merge params
                              {:active-page     "preview"
                               :home            (when index-page? true)
                               :selmer/context  (cryogen-io/path "/" blog-prefix "/")
                               :posts           posts
                               :prev-uri        prev
                               :next-uri        next})))))))

(defn add-description
  "Add plain text `:description` to the page/post for use in meta description etc."
  [{:keys [blocks-per-preview description-include-elements]
    :or   {description-include-elements #{:p :h1 :h2 :h3 :h4 :h5 :h6}}}
   page]
  (update
    page :description
    #(cond
       (false? %) nil  ;; if set via page meta to false, do not set
       % %    ;; if set via page meta, use it
       :else (->> (enlive/select
                    (preview-dom blocks-per-preview (:content-dom page))
                    [(set description-include-elements)])
                  (util/enlive->plain-text)))))
(defn compile-index
  "Compiles the index page into html and spits it out into the public folder"
  [{:keys [blog-prefix debug? home-page] :as params}]
  (println (blue "compiling index"))
  (let [uri (page-uri "index.html" params)]
    (when debug?
      (print-debug-info meta))
    (write-html uri
                params
                (render-file (str "/html/" (:layout home-page))
                             (merge params
                                    {:active-page       "home"
                                     :home              true
                                     :selmer/context    (cryogen-io/path "/" blog-prefix "/")
                                     :uri               uri
                                     (:type home-page)  home-page})))))

(defn compile-archives
  "Compiles the archives page into html and spits it out into the public folder"
  [{:keys [blog-prefix] :as params} posts]
  (println (blue "compiling archives"))
  (let [uri (page-uri "archives.html" params)]
    (write-html uri
                params
                (render-file "/html/archives.html"
                             (merge params
                                    {:active-page     "archives"
                                     :archives        true
                                     :groups          (group-for-archive posts)
                                     :selmer/context  (cryogen-io/path "/" blog-prefix "/")
                                     :uri             uri})))))

(defn compile-authors
  "For each author, creates a page with filtered posts."
  [{:keys [blog-prefix author-root-uri author] :as params} posts]
  (println (blue "compiling authors"))
  (cryogen-io/create-folder (cryogen-io/path "/" blog-prefix author-root-uri))
  ;; if the post author is empty defaults to config's :author
  (doseq [{:keys [author posts]} (group-for-author posts author)]
    (let [uri (page-uri (str author ".html") :author-root-uri params)]
      (println "-->" (cyan uri))
      (write-html uri
                  params
                  (render-file "/html/author.html"
                               (merge params
                                      {:author          author
                                       :groups          (group-for-archive posts)
                                       :selmer/context  (cryogen-io/path "/" blog-prefix "/")
                                       :uri             uri}))))))

(defn tag-posts
  "Converts the tags in each post into links"
  [posts config]
  (map #(update-in % [:tags] (partial map (partial tag-info config))) posts))

(defn- content-dir?
  "Checks that the dir exists in the content directory."
  [dir]
  (.isDirectory (io/file (cryogen-io/path content-root dir))))

(defn- markup-entries [post-root page-root]
  (let [entries (for [mu (m/markups)
                      t  (distinct [post-root page-root])]
                  [(cryogen-io/path (m/dir mu) t) t])]
    (apply concat entries)))

(defn copy-resources-from-markup-folders
  "Copy resources from markup folders. This does not copy the markup entries."
  [{:keys [post-root page-root] :as config}]
  (let [folders (->> (markup-entries post-root page-root)
                     (filter content-dir?))]
    (cryogen-io/copy-resources
     content-root
     (merge config
            {:resources     folders
             :ignored-files (map #(re-pattern-from-exts (m/exts %)) (m/markups))}))))

(defn compile-assets
  "Generates all the html and copies over resources specified in the config.

  Params:
   - `overrides-and-hooks` - may contain overrides for `config.edn`; anything
      here will be available to the page templates, except for the following special
                parameters:
     - `:extend-params-fn` - a function (`params`, `site-data`) -> `params` -
                             use it to derive/add additional params for templates
     - `:postprocess-article-html-fn` - a function (`article`, `params`) -> `article`
                             called after the `:content` has been rendered to HTML and
                              right before it is written to the disk. Example fn:
                              `(fn postprocess [article params] (update article :content selmer.parser/render params))`
     - `:update-article-fn` - a function (`article`, `config`) -> `article` to update a
                            parsed page/post. Return nil to exclude it.
     - `changeset` - as supplied by
                   [[cryogen-core.watcher/start-watcher-for-changes!]] to its callback
                   for incremental compilation, see [[only-changed-files-filter]]
                   for details

  Note on terminology:
   - `article` - a post or page data (including its title, content, etc.)
   - `config` - the site-wide configuration ± from `config.edn` and the provided overrides
   - `params` - `config` + content such as `:pages` etc.
   - `site-data` - a subset of the site content such as `:pages`, `:posts` - see the code below"
  ([]
   (compile-assets {}))
  ([{:keys [extend-params-fn update-article-fn]
     :or   {extend-params-fn            (fn [params _] params)
            update-article-fn           (fn [article _] article)}
     :as   overrides-and-hooks}
    changeset]
   (println (green "compiling assets..."))
   (when-not (empty? overrides-and-hooks)
     (println (yellow "overriding config.edn with:"))
     (pprint overrides-and-hooks))
   (let [inc-compile? (seq changeset) ; Don't recompile unchanged posts/pages (ie. most time-consuming)
         inc-compile-filter (only-changed-files-filter changeset)
         overrides    (dissoc overrides-and-hooks
                              :extend-params-fn :update-article-fn)
         {:keys [^String site-url blog-prefix rss-name recent-posts keep-files ignored-files previews? author-root-uri theme]
          :as   config} (resolve-config overrides)
         posts        (->> (read-posts config inc-compile-filter)
                           (add-prev-next)
                           (map klipse/klipsify)
                           (map (partial add-description config))
                           (map #(update-article-fn % config))
                           (remove nil?))
         posts-by-tag (group-by-tags posts)
         posts        (tag-posts posts config)
         latest-posts (->> posts (take recent-posts) vec)
         pages        (->> (read-pages config inc-compile-filter)
                           (map klipse/klipsify)
                           (map (partial add-description config))
                           (map #(update-article-fn % config))
                           (remove nil?))
         home-page    (->> pages
                           (filter #(boolean (:home? %)))
                           (first))
         other-pages  (->> pages
                           (remove #{home-page})
                           (add-prev-next))
         [navbar-pages
          sidebar-pages] (group-pages other-pages)
         params0      (merge
                       config
                       {:today         (Date.)
                        :title         (:site-title config)
                        :active-page   "home"
                        :tags          (map (partial tag-info config) (keys posts-by-tag))
                        :latest-posts  latest-posts
                        :navbar-pages  navbar-pages
                        :sidebar-pages sidebar-pages
                        :home-page     (if home-page
                                         home-page
                                         (assoc (first latest-posts) :layout "home.html"))
                        :archives-uri  (page-uri "archives.html" config)
                        :index-uri     (page-uri "index.html" config)
                        :tags-uri      (page-uri "tags.html" config)
                        :rss-uri       (cryogen-io/path "/" blog-prefix rss-name)
                        :site-url      (if (.endsWith site-url "/") (.substring site-url 0 (dec (count site-url))) site-url)})
         params       (extend-params-fn
                        params0
                        {:posts posts
                         :pages pages
                         :posts-by-tag posts-by-tag
                         :navbar-pages navbar-pages
                         :sidebar-pages sidebar-pages})]

     (selmer.parser/set-resource-path!
       (util/file->url (io/as-file (cryogen-io/path "themes" theme))))
     (cryogen-io/set-public-path! (:public-dest config))

     (when-not inc-compile?
       (cryogen-io/wipe-public-folder keep-files))
     (println (blue "compiling sass"))
     (sass/compile-sass->css! config)
     (println (blue "copying theme resources"))
     (cryogen-io/copy-resources-from-theme config)
     (println (blue "copying resources"))
     (cryogen-io/copy-resources "content" config)
     (copy-resources-from-markup-folders config)
     (compile-pages params other-pages)
     (compile-posts params posts)
     (compile-tags params posts-by-tag)
     (compile-tags-page params)
     (if previews?
       (compile-preview-pages params posts)
       (compile-index params))
     (compile-archives params posts)
     (when author-root-uri
       (println (blue "generating authors views"))
       (compile-authors params posts))
     (println (blue "generating site map"))
     (->> (sitemap/generate site-url config)
          (cryogen-io/create-file (cryogen-io/path "/" blog-prefix "sitemap.xml")))
     (println (blue "generating main rss"))
     (->> (rss/make-channel config posts)
          (cryogen-io/create-file (cryogen-io/path "/" blog-prefix rss-name)))
     (if (:rss-filters config) (println (blue "generating filtered rss")))
     (rss/make-filtered-channels config posts-by-tag)
     (when inc-compile?
       (println (yellow "BEWARE: Incremental compilation, things are missing. Make a full build before publishing."))))))

(defn compile-assets-timed
  "See the docstring for [[compile-assets]]"
  ([] (compile-assets-timed {}))
  ([config] (compile-assets-timed config {}))
  ([config changeset]
   (time
    (try
      (compile-assets config changeset)
      (catch Exception e
        (if (or (instance? IllegalArgumentException e)
                (instance? clojure.lang.ExceptionInfo e))
          (println (red "Error:") (yellow (.getMessage e)))
          (write-exception e)))))))

(comment
  (def *config (resolve-config {}))

  ;; Build and copy only styles & theme
  (do
    (sass/compile-sass->css! *config)
    (cryogen-io/copy-resources-from-theme *config))

  ;; Build a single page (quicker than all)
  (compile-assets
    ;; Insert the prefix and suffix of the only file you _want_ to process
    {:ignored-files [#"^(?!2019-12-12-nrepl-).*\.asc"]})

  nil)
