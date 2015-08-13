(ns crawler-ui.core
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [ajax.core :refer [POST GET]]
            [clojure.string :as string]
            [cljs.core.async :as as]))

(enable-console-print!)

(defonce app-state (atom {:results []
                      :loading false
                      :statistics {}
                      :statistics-ping (as/chan)
                      :error false
                      :query ""}))

(defn format-size [bytes]
  (let [[prefix base] (cond (< bytes (* 1.5 (Math/pow 1024 1))) ["B" 1]
                            (< bytes (* 1.5 (Math/pow 1024 2))) ["KB" 1024]
                            (< bytes (* 1.5 (Math/pow 1024 3))) ["MB" (Math/pow 1024 2)]
                            (< bytes (* 1.5 (Math/pow 1024 4))) ["GB" (Math/pow 1024 3)]
                            :default ["TB" (Math/pow 1024 4)])]
       (str (quot bytes base) prefix)))

(defn statistics-ping-loop [chan]
  (go-loop []
    (as/put! chan :ping)
    (as/<! (as/timeout 1000))
    (recur)))

(defn result-view [file owner]
  (reify
    om/IRender
    (render [this]
      (dom/tr nil
              (dom/td nil (format-size (get file "size")))
              (dom/td nil
                      (dom/a #js {:href (get file "url")}
                             (js/decodeURI (get file "url"))))))))

(defn results-view [data owner]
  (reify
    om/IRender
    (render [this]
      (dom/div nil
        (dom/h2 nil "Results")
        (dom/table nil
                   (dom/thead nil
                              (dom/tr nil
                                      (dom/th nil "Size")
                                      (dom/th nil "Url")))
                   (apply dom/tbody nil
                          (om/build-all result-view (sort-by #(get % "name") (:results data)))))))))

(defn statistics-value-view [value owner]
  (reify
    om/IRender
    (render [this]
      (dom/tr nil
              (dom/td nil (get value "ip"))
              (dom/td nil (get value "files"))
              (dom/td nil (format-size (get value "size")))))))

(defn sort-statistics [data]
  (sort-by #(mapv js/parseInt (string/split (first %) #"\.")) data))

(defn statistics-view [data owner]
  (reify
    om/IRender
    (render [this]
      (dom/div nil
               (dom/h3 nil "Statistics")
               (dom/table nil
                          (dom/thead nil
                                     (dom/tr nil
                                             (dom/th nil "Server")
                                             (dom/th nil "Files")
                                             (dom/th nil "Size")))
                          (apply dom/tbody nil
                                 (om/build-all
                                  statistics-value-view
                                  (sort-statistics (:statistics data)))))))))

(defn search-and-save [query data]
  (om/update! data :loading true)
  (POST (str "http://" js/location.hostname ":8081")
        {:params {:query query}
         :format :json
         :error-handler (fn [response]
                          (om/update! data :error true)
                          (om/update! data :loading false))
         :handler (fn [response]
                    (om/update! data :error false)
                    (om/update! data :results response)
                    (om/update! data :loading false))}))

(defn get-statistics []
  (let [chan (as/chan)]
    (GET (str "http://" js/location.hostname ":8081")
         {:format :json
          :error-handler (fn [& body] (as/put! chan :error))
          :handler (fn [response] (as/put! chan response))})
    chan))

(defn statistics-loop [data]
  (go-loop []
    (as/<! (:statistics-ping data))
    (let [result (<! (get-statistics))]
      (when-not (= :error result)
        (om/update! data :statistics result))
      (recur))))

(defn input-view [data owner]
  (reify
    om/IRender
    (render [this]
      (let [on-key-press (fn [e]
                           (let [input (om/get-node owner "query-input")
                                 query (-> input .-value)]
                             (when (and (= 13 (-> e .-charCode))
                                        (not= "" (string/trim query)))
                               (om/update! data :query query)
                               (search-and-save query data))))]
        (dom/div nil
                 (dom/div nil "Query accepts standard lucene queries: "
                          (dom/a #js {:href "https://lucene.apache.org/core/2_9_4/queryparsersyntax.html"} "click here"))
                 (dom/div nil "Example queries: \"bdsm AND .mp4$\" (All bdsm files that end in .mp4), \"ebooks AND algorithms\" (ebooks about algorithms), \"apocalyptica AND .mp3$\" (files with apocalyptica in it that end in .mp3)")
                 (dom/div nil "Press enter for search. Results are limited to 10000, if your browser dies, it's your fault.")
                 (dom/div nil
                          (dom/input #js {:type :text
                                          :placeholder "query"
                                          :ref "query-input"
                                          :onKeyPress on-key-press}
                                     (:query data))
                          (when (:loading data)
                            (dom/span nil "Loading..."))
                          (when (:error data)
                            (dom/span nil "Error :("))))))))

()

(defn main []
  (statistics-ping-loop (:statistics-ping @app-state))
  (om/root
   (fn [app owner]
     (reify
       om/IRender
       (render [_]
         (statistics-loop app)
         (dom/div nil
                  (om/build input-view app)
                  (om/build results-view app)
                  (om/build statistics-view app)))))
   app-state
   {:target (. js/document (getElementById "app"))})
  )


