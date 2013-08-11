(ns ascd.core
  (:use [cheshire.core]
        [clojure.tools.logging])
  (:use [clojure.set])
  (:import [org.webbitserver
            WebServer
            WebServers
            WebSocketHandler
            handler.StaticFileHandler])
  (:gen-class :main true))

(def players (atom #{}))

(defn room-active [room-id]
  (< 0 (count @players)))

(defn start-room []
  (info "Engine started")
  (future (loop []
            (info "Tick")
            (Thread/sleep 1000)
            (when (room-active 1) (recur)))
    (info "Engine done")))

(defn uid [ws]
  (str (.data ws "ip") ":" (.data ws "port")))

(defn ipaddr [ws]
  (-> (.httpRequest ws) .remoteAddress .getAddress .getHostAddress))

(defn port [ws]
  (-> (.httpRequest ws) .remoteAddress .getPort))

(defn send-others [self msg]
  (let [json (encode msg)
        idx (index @players [:uid])]
    (doseq [player (difference @players (idx {:uid (uid self)}))]
      (.send (:socket player) json))))

(defn handle-update [msg ws]
  (send-others ws {:id "UPDATE"
                   :channel (msg "channel")
                   :from (uid ws)
                   :data (msg "data")}))

(defn handle-dead [msg ws]
  (info (format "%s -> DEAD (killed by %s)" (uid ws) ((msg "data") "by")))
  (send-others ws {:id "DEAD"
                   :channel 0
                   :from (uid ws)
                   :data (msg "data")}))

(defn on-open [ws]
  (.data ws "ip" (ipaddr ws))
  (.data ws "port" (port ws))
  (if (not (room-active 1))
    (start-room))
  (swap! players #(conj % {:uid (uid ws) :socket ws}))
  (info (format "%s -> JOIN" (uid ws))))

(defn on-close [ws]
  (info (format "%s -> LEAVE" (uid ws)))
  (send-others ws {:id "LEAVE"
                   :channel 0
                   :from (uid ws)})
  (swap! players #(difference % ((index % [:uid]) {:uid (uid ws)}))))

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
