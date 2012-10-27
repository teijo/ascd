(ns ascd.core
  (:use [cheshire.core]
        [clojure.tools.logging])
  (:import [org.webbitserver
            WebServer
            WebServers
            WebSocketHandler
            handler.StaticFileHandler])
  (:gen-class :main true))

(def players (atom #{}))

(defn uid [ws]
  (str (.data ws "ip") ":" (.data ws "port")))

(defn ipaddr [ws]
  (-> (.httpRequest ws) .remoteAddress .getAddress .getHostAddress))

(defn port [ws]
  (-> (.httpRequest ws) .remoteAddress .getPort))

(defn send-others [self msg]
  (let [json (encode msg)]
    (doseq [ws (disj @players self)]
      (.send ws json))))

(defn handle-update [msg ws]
  (send-others ws {:id "UPDATE"
                   :from (uid ws)
                   :data (msg "data")}))

(defn handle-dead [msg ws]
  (info (format "%s -> DEAD (killed by %s)" (uid ws) ((msg "data") "by")))
  (send-others ws {:id "DEAD"
                   :from (uid ws)
                   :data (msg "data")}))

(defn on-open [ws]
  (swap! players #(conj % ws))
  (.data ws "ip" (ipaddr ws))
  (.data ws "port" (port ws))
  (info (format "%s -> JOIN" (uid ws))))

(defn on-close [ws]
  (info (format "%s -> LEAVE" (uid ws)))
  (send-others ws {:id "LEAVE"
                   :from (uid ws)})
  (swap! players #(disj % ws)))

(defn on-message [ws json]
  (let [msg (decode json)
        msg-type (msg "id")]
    (cond
      (= msg-type "UPDATE") (handle-update msg ws),
      (= msg-type "DEAD") (handle-dead msg ws))))

(defn -main [& m]
  (def server (WebServers/createWebServer (Integer/parseInt (System/getenv "PORT"))))

  (.add server
    (proxy [StaticFileHandler] ["./web"]))

  (.add server "/game"
    (proxy [WebSocketHandler] []
      (onOpen [ws] (on-open ws))
      (onClose [ws] (on-close ws))
      (onMessage [ws json] (on-message ws json))))

  (.start server))
