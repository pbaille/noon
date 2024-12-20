(ns noon.externals
  (:require [clojure.java.shell :as shell]
            [noon.utils.misc :as u]))

(def MUSESCORE_BIN "mscore")
(def FLUIDSYNTH_BIN "fluidsynth")
(def FFMPEG_BIN "ffmpeg")

(defn handle-externals
  "Process external parts of the `noon.score/noon` evaluation."
  [{:as _files :keys [mp3-file midi-file xml-file pdf-file]}]
  (when (and mp3-file
             (zero? (:exit (shell/sh "which" FLUIDSYNTH_BIN)))
             (zero? (:exit (shell/sh "which" FFMPEG_BIN))))
    (shell/sh "sh" "-c" (str FLUIDSYNTH_BIN " -a alsa -T raw -F -"
                             " ./resources/midi/soundfonts/choriumreva.sf2"
                             " ./" midi-file
                             " | " FFMPEG_BIN " -f s32le -i -"
                             " ./" mp3-file)))

  (when (zero? (:exit (shell/sh "which" MUSESCORE_BIN)))
    (if xml-file
      (shell/sh MUSESCORE_BIN "--export-to" xml-file midi-file))

    (when pdf-file
      (shell/sh MUSESCORE_BIN xml-file "-o" pdf-file)
      (u/copy-file pdf-file "last.pdf"))))
