(ns noon.data.GM)

(def instruments
  [{:val 0, :name "Acoustic Grand Piano", :group "Piano"}
   {:val 1, :name "Bright Acoustic Piano", :group "Piano"}
   {:val 2, :name "Electric Grand Piano", :group "Piano"}
   {:val 3, :name "Honky-tonk Piano", :group "Piano"}
   {:val 4, :name "Electric Piano 1", :group "Piano"}
   {:val 5, :name "Electric Piano 2", :group "Piano"}
   {:val 6, :name "Harpsichord", :group "Piano"}
   {:val 7, :name "Clavinet", :group "Piano"}
   {:val 8, :name "Celesta", :group "Chromatic Percussion"}
   {:val 9, :name "Glockenspiel", :group "Chromatic Percussion"}
   {:val 10, :name "Music Box", :group "Chromatic Percussion"}
   {:val 11, :name "Vibraphone", :group "Chromatic Percussion"}
   {:val 12, :name "Marimba", :group "Chromatic Percussion"}
   {:val 13, :name "Xylophone", :group "Chromatic Percussion"}
   {:val 14, :name "Tubular Bells", :group "Chromatic Percussion"}
   {:val 15, :name "Dulcimer", :group "Chromatic Percussion"}
   {:val 16, :name "Drawbar Organ", :group "Organ"}
   {:val 17, :name "Percussive Organ", :group "Organ"}
   {:val 18, :name "Rock Organ", :group "Organ"}
   {:val 19, :name "Church Organ", :group "Organ"}
   {:val 20, :name "Reed Organ", :group "Organ"}
   {:val 21, :name "Accordion", :group "Organ"}
   {:val 22, :name "Harmonica", :group "Organ"}
   {:val 23, :name "Tango Accordion", :group "Organ"}
   {:val 24, :name "Acoustic Guitar (nylon)", :group "Guitar"}
   {:val 25, :name "Acoustic Guitar (steel)", :group "Guitar"}
   {:val 26, :name "Electric Guitar (jazz)", :group "Guitar"}
   {:val 27, :name "Electric Guitar (clean)", :group "Guitar"}
   {:val 28, :name "Electric Guitar (muted)", :group "Guitar"}
   {:val 29, :name "Overdriven Guitar", :group "Guitar"}
   {:val 30, :name "Distortion Guitar", :group "Guitar"}
   {:val 31, :name "Guitar Harmonics", :group "Guitar"}
   {:val 32, :name "Acoustic Bass", :group "Bass"}
   {:val 33, :name "Electric Bass (finger)", :group "Bass"}
   {:val 34, :name "Electric Bass (pick)", :group "Bass"}
   {:val 35, :name "Fretless Bass", :group "Bass"}
   {:val 36, :name "Slap Bass 1", :group "Bass"}
   {:val 37, :name "Slap Bass 2", :group "Bass"}
   {:val 38, :name "Synth Bass 1", :group "Bass"}
   {:val 39, :name "Synth Bass 2", :group "Bass"}
   {:val 40, :name "Violin", :group "Strings"}
   {:val 41, :name "Viola", :group "Strings"}
   {:val 42, :name "Cello", :group "Strings"}
   {:val 43, :name "Contrabass", :group "Strings"}
   {:val 44, :name "Tremolo Strings", :group "Strings"}
   {:val 45, :name "Pizzicato Strings", :group "Strings"}
   {:val 46, :name "Orchestral Harp", :group "Strings"}
   {:val 47, :name "Timpani", :group "Strings"}
   {:val 48, :name "String Ensemble 1", :group "Ensemble"}
   {:val 49, :name "String Ensemble 2", :group "Ensemble"}
   {:val 50, :name "Synth Strings 1", :group "Ensemble"}
   {:val 51, :name "Synth Strings 2", :group "Ensemble"}
   {:val 52, :name "Choir Aahs", :group "Ensemble"}
   {:val 53, :name "Voice Oohs", :group "Ensemble"}
   {:val 54, :name "Synth Choir", :group "Ensemble"}
   {:val 55, :name "Orchestra Hit", :group "Ensemble"}
   {:val 56, :name "Trumpet", :group "Brass"}
   {:val 57, :name "Trombone", :group "Brass"}
   {:val 58, :name "Tuba", :group "Brass"}
   {:val 59, :name "Muted Trumpet", :group "Brass"}
   {:val 60, :name "French Horn", :group "Brass"}
   {:val 61, :name "Brass Section", :group "Brass"}
   {:val 62, :name "Synth Brass 1", :group "Brass"}
   {:val 63, :name "Synth Brass 2", :group "Brass"}
   {:val 64, :name "Soprano Sax", :group "Reed"}
   {:val 65, :name "Alto Sax", :group "Reed"}
   {:val 66, :name "Tenor Sax", :group "Reed"}
   {:val 67, :name "Baritone Sax", :group "Reed"}
   {:val 68, :name "Oboe", :group "Reed"}
   {:val 69, :name "English Horn", :group "Reed"}
   {:val 70, :name "Bassoon", :group "Reed"}
   {:val 71, :name "Clarinet", :group "Reed"}
   {:val 72, :name "Piccolo", :group "Pipe"}
   {:val 73, :name "Flute", :group "Pipe"}
   {:val 74, :name "Recorder", :group "Pipe"}
   {:val 75, :name "Pan Flute", :group "Pipe"}
   {:val 76, :name "Blown bottle", :group "Pipe"}
   {:val 77, :name "Shakuhachi", :group "Pipe"}
   {:val 78, :name "Whistle", :group "Pipe"}
   {:val 79, :name "Ocarina", :group "Pipe"}
   {:val 80, :name "Lead 1 (square)", :group "Synth Lead"}
   {:val 81, :name "Lead 2 (sawtooth)", :group "Synth Lead"}
   {:val 82, :name "Lead 3 (calliope)", :group "Synth Lead"}
   {:val 83, :name "Lead 4 (chiff)", :group "Synth Lead"}
   {:val 84, :name "Lead 5 (charang)", :group "Synth Lead"}
   {:val 85, :name "Lead 6 (voice)", :group "Synth Lead"}
   {:val 86, :name "Lead 7 (fifths)", :group "Synth Lead"}
   {:val 87, :name "Lead 8 (bass + lead)", :group "Synth Lead"}
   {:val 88, :name "Pad 1 (new age)", :group "Synth Pad"}
   {:val 89, :name "Pad 2 (warm)", :group "Synth Pad"}
   {:val 90, :name "Pad 3 (polysynth)", :group "Synth Pad"}
   {:val 91, :name "Pad 4 (choir)", :group "Synth Pad"}
   {:val 92, :name "Pad 5 (bowed)", :group "Synth Pad"}
   {:val 93, :name "Pad 6 (metallic)", :group "Synth Pad"}
   {:val 94, :name "Pad 7 (halo)", :group "Synth Pad"}
   {:val 95, :name "Pad 8 (sweep)", :group "Synth Pad"}
   {:val 96, :name "FX 1 (rain)", :group "Synth Effects"}
   {:val 97, :name "FX 2 (soundtrack)", :group "Synth Effects"}
   {:val 98, :name "FX 3 (crystal)", :group "Synth Effects"}
   {:val 99, :name "FX 4 (atmosphere)", :group "Synth Effects"}
   {:val 100, :name "FX 5 (brightness)", :group "Synth Effects"}
   {:val 101, :name "FX 6 (goblins)", :group "Synth Effects"}
   {:val 102, :name "FX 7 (echoes)", :group "Synth Effects"}
   {:val 103, :name "FX 8 (sci-fi)", :group "Synth Effects"}
   {:val 104, :name "Sitar", :group "Ethnic"}
   {:val 105, :name "Banjo", :group "Ethnic"}
   {:val 106, :name "Shamisen", :group "Ethnic"}
   {:val 107, :name "Koto", :group "Ethnic"}
   {:val 108, :name "Kalimba", :group "Ethnic"}
   {:val 109, :name "Bagpipe", :group "Ethnic"}
   {:val 110, :name "Fiddle", :group "Ethnic"}
   {:val 111, :name "Shanai", :group "Ethnic"}
   {:val 112, :name "Tinkle Bell", :group "Percussive"}
   {:val 113, :name "Agogo", :group "Percussive"}
   {:val 114, :name "Steel Drums", :group "Percussive"}
   {:val 115, :name "Woodblock", :group "Percussive"}
   {:val 116, :name "Taiko Drum", :group "Percussive"}
   {:val 117, :name "Melodic Tom", :group "Percussive"}
   {:val 118, :name "Synth Drum", :group "Percussive"}
   {:val 119, :name "Reverse Cymbal", :group "Percussive"}
   {:val 120, :name "Guitar Fret Noise", :group "Sound effects"}
   {:val 121, :name "Breath Noise", :group "Sound effects"}
   {:val 122, :name "Seashore", :group "Sound effects"}
   {:val 123, :name "Bird Tweet", :group "Sound effects"}
   {:val 124, :name "Telephone Ring", :group "Sound effects"}
   {:val 125, :name "Helicopter", :group "Sound effects"}
   {:val 126, :name "Applause", :group "Sound effects"}
   {:val 127, :name "Gunshot", :group "Sound effects"}])
