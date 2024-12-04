(ns noon.data.chromaphone)

(def banks
  [{:idx 0,
    :name "Mallets",
    :key :mallets,
    :elements
    [{:idx 0, :name "Fairmount Marimba", :key :fairmount-marimba}
     {:idx 1, :name "Markemeer Marimba", :key :markemeer-marimba}
     {:idx 2, :name "Soft Marimba", :key :soft-marimba}
     {:idx 3, :name "Airy Marimba", :key :airy-marimba}
     {:idx 4, :name "Tricky Marimba", :key :tricky-marimba}
     {:idx 5, :name "Bouncy Marimba", :key :bouncy-marimba}
     {:idx 6, :name "Live Vibes", :key :live-vibes}
     {:idx 7, :name "Royal Vibe", :key :royal-vibe}
     {:idx 8, :name "French Vibe", :key :french-vibe}
     {:idx 9, :name "Kiss & Run Vibe", :key :kiss-&-run-vibe}
     {:idx 10, :name "Rubber Mallet Xylo", :key :rubber-mallet-xylo}
     {:idx 11, :name "Acrylic Mallet Xylo", :key :acrylic-mallet-xylo}
     {:idx 12, :name "Gum Balafon", :key :gum-balafon}
     {:idx 13, :name "Béné Balafon", :key :béné-balafon}
     {:idx 14, :name "Senza 1", :key :senza-1}
     {:idx 15, :name "Senza 2", :key :senza-2}
     {:idx 16, :name "Glocky", :key :glocky}
     {:idx 17, :name "Short Glock", :key :short-glock}
     {:idx 18, :name "Child Glock", :key :child-glock}
     {:idx 19, :name "Glass Glock", :key :glass-glock}
     {:idx 20, :name "Vibraglock", :key :vibraglock}
     {:idx 21, :name "Glock and Bells", :key :glock-and-bells}
     {:idx 22, :name "Celestian", :key :celestian}
     {:idx 23, :name "Nyunga Kalimba", :key :nyunga-kalimba}
     {:idx 24, :name "Okeme Kalimba", :key :okeme-kalimba}
     {:idx 25, :name "Crystal Kalimba", :key :crystal-kalimba}
     {:idx 26, :name "Huru Kalimba", :key :huru-kalimba}
     {:idx 27, :name "Fort Chambly Kalimba", :key :fort-chambly-kalimba}
     {:idx 28, :name "Njari Kalimba", :key :njari-kalimba}
     {:idx 29, :name "Cheap Kalimba", :key :cheap-kalimba}
     {:idx 30, :name "Steelpan", :key :steelpan}
     {:idx 31, :name "Glockenstring", :key :glockenstring}
     {:idx 32, :name "Woodworks", :key :woodworks}
     {:idx 33, :name "Ethniticity", :key :ethniticity}
     {:idx 34, :name "Woodpecker", :key :woodpecker}
     {:idx 35, :name "Clickblocks", :key :clickblocks}
     {:idx 36, :name "French Cancan", :key :french-cancan}
     {:idx 37, :name "Resophone 1", :key :resophone-1}
     {:idx 38, :name "Resophone 2", :key :resophone-2}
     {:idx 39, :name "Electric Wood", :key :electric-wood}
     {:idx 40, :name "Electric Marimba", :key :electric-marimba}
     {:idx 41, :name "Electronic Vibes", :key :electronic-vibes}
     {:idx 42, :name "Multi Marimba", :key :multi-marimba}
     {:idx 43, :name "Glass Grains", :key :glass-grains}
     {:idx 44, :name "Glass Breath", :key :glass-breath}
     {:idx 45, :name "Dizzy Marimba", :key :dizzy-marimba}
     {:idx 46, :name "Cannelloni", :key :cannelloni}
     {:idx 47, :name "Haunted Childhood", :key :haunted-childhood}
     {:idx 48, :name "Vibraflute", :key :vibraflute}]}
   {:idx 1,
    :name "Percussions",
    :key :percussions,
    :elements
    [{:idx 0, :name "Snappy Percs 1", :key :snappy-percs-1}
     {:idx 1, :name "Snappy Percs 2", :key :snappy-percs-2}
     {:idx 2, :name "Arabian Perc", :key :arabian-perc}
     {:idx 3, :name "Brush Tam", :key :brush-tam}
     {:idx 4, :name "Bongo High", :key :bongo-high}
     {:idx 5, :name "Bongo Low 1", :key :bongo-low-1}
     {:idx 6, :name "Bongo Low 2", :key :bongo-low-2}
     {:idx 7, :name "Multi-Bongo", :key :multi-bongo}
     {:idx 8, :name "Tama Tama", :key :tama-tama}
     {:idx 9, :name "Davul", :key :davul}
     {:idx 10, :name "Ground and Sun", :key :ground-and-sun}
     {:idx 11,
      :name "Drum from a Lost Tribe",
      :key :drum-from-a-lost-tribe}
     {:idx 12, :name "African Drum", :key :african-drum}
     {:idx 13, :name "Talking Drum", :key :talking-drum}
     {:idx 14, :name "Clay Drum", :key :clay-drum}
     {:idx 15, :name "Percufun 1", :key :percufun-1}
     {:idx 16, :name "Percufun 2", :key :percufun-2}
     {:idx 17, :name "Percufun 3", :key :percufun-3}
     {:idx 18, :name "Percu Manual 1", :key :percu-manual-1}
     {:idx 19, :name "Indian Percussion", :key :indian-percussion}
     {:idx 20, :name "Tabla Town", :key :tabla-town}
     {:idx 21, :name "Waverly Tabla Low", :key :waverly-tabla-low}
     {:idx 22, :name "Waverly Tabla Middle", :key :waverly-tabla-middle}
     {:idx 23, :name "Waverly Tabla High", :key :waverly-tabla-high}
     {:idx 24, :name "Metal Tongue Drum", :key :metal-tongue-drum}
     {:idx 25, :name "Timpani", :key :timpani}
     {:idx 26, :name "Double 'Brane 1", :key :double-'brane-1}
     {:idx 27, :name "Double 'Brane 2", :key :double-'brane-2}
     {:idx 28, :name "Double 'Brane 3", :key :double-'brane-3}
     {:idx 29, :name "Gypsy Fairy", :key :gypsy-fairy}
     {:idx 30, :name "Mont-Royal 1", :key :mont-royal-1}
     {:idx 31, :name "Mont-Royal 2", :key :mont-royal-2}
     {:idx 32,
      :name "Vinyl Sampled African Percussions",
      :key :vinyl-sampled-african-percussions}
     {:idx 33, :name "Afro Cajón", :key :afro-cajón}
     {:idx 34, :name "Claves", :key :claves}
     {:idx 35, :name "Electro Clave", :key :electro-clave}
     {:idx 36, :name "Woodblocks", :key :woodblocks}
     {:idx 37, :name "Plastiwhaker", :key :plastiwhaker}
     {:idx 38, :name "Big Kit Cowbell", :key :big-kit-cowbell}
     {:idx 39, :name "Pig Bell", :key :pig-bell}
     {:idx 40, :name "Horse Bell", :key :horse-bell}
     {:idx 41, :name "Goat Bell", :key :goat-bell}
     {:idx 42, :name "Duck Bell", :key :duck-bell}
     {:idx 43, :name "Sheep Bell", :key :sheep-bell}
     {:idx 44, :name "Cinematic Snare Clap", :key :cinematic-snare-clap}
     {:idx 45, :name "Grindy Clap", :key :grindy-clap}
     {:idx 46, :name "Funk Clap 1", :key :funk-clap-1}
     {:idx 47, :name "Funk Clap 2", :key :funk-clap-2}
     {:idx 48, :name "Clap Kameo", :key :clap-kameo}
     {:idx 49, :name "Clap 1", :key :clap-1}
     {:idx 50, :name "Clap 2", :key :clap-2}
     {:idx 51, :name "Clap 3", :key :clap-3}
     {:idx 52, :name "Big Kit Hand Clap", :key :big-kit-hand-clap}
     {:idx 53, :name "Big Kit Finger Snap", :key :big-kit-finger-snap}
     {:idx 54, :name "Simili-Tambourine", :key :simili-tambourine}
     {:idx 55, :name "Af Shaker 1", :key :af-shaker-1}
     {:idx 56, :name "Af Shaker 2", :key :af-shaker-2}
     {:idx 57, :name "Af Shaker 3", :key :af-shaker-3}
     {:idx 58, :name "Af Shaker 4", :key :af-shaker-4}
     {:idx 59, :name "SD's Shaker", :key :sd's-shaker}
     {:idx 60, :name "Karkabou 1", :key :karkabou-1}
     {:idx 61, :name "Karkabou 2", :key :karkabou-2}
     {:idx 62, :name "Karkabou 3", :key :karkabou-3}
     {:idx 63, :name "Spur Perc", :key :spur-perc}
     {:idx 64, :name "Short Frog", :key :short-frog}
     {:idx 65, :name "Long Frog", :key :long-frog}
     {:idx 66, :name "Vibraslap", :key :vibraslap}
     {:idx 67, :name "Western Perc", :key :western-perc}
     {:idx 68, :name "Tribal Initiation", :key :tribal-initiation}
     {:idx 69, :name "Frozen Grooves", :key :frozen-grooves}
     {:idx 70, :name "Rain Pan Drumming", :key :rain-pan-drumming}
     {:idx 71, :name "Circo Fantasma", :key :circo-fantasma}
     {:idx 72, :name "Cinematic Drum 1", :key :cinematic-drum-1}
     {:idx 73, :name "Cinematic Drum 2", :key :cinematic-drum-2}]}
   {:idx 2,
    :name "Kicks",
    :key :kicks,
    :elements
    [{:idx 0, :name "OCD Kick 1", :key :ocd-kick-1}
     {:idx 1, :name "OCD Kick 2", :key :ocd-kick-2}
     {:idx 2, :name "OCD Kick 3", :key :ocd-kick-3}
     {:idx 3, :name "OCD Kick 4", :key :ocd-kick-4}
     {:idx 4, :name "OCD Kick 5", :key :ocd-kick-5}
     {:idx 5, :name "OCD Kick 6", :key :ocd-kick-6}
     {:idx 6, :name "Léa's Kick", :key :léa's-kick}
     {:idx 7, :name "Clark Kick", :key :clark-kick}
     {:idx 8, :name "Plain Bass Drum", :key :plain-bass-drum}
     {:idx 9, :name "Jazz Kick", :key :jazz-kick}
     {:idx 10, :name "Jazz O Kick", :key :jazz-o-kick}
     {:idx 11, :name "Kick Jazzet", :key :kick-jazzet}
     {:idx 12, :name "Kick Day", :key :kick-day}
     {:idx 13, :name "Tambour Kick", :key :tambour-kick}
     {:idx 14, :name "Kick Medium", :key :kick-medium}
     {:idx 15, :name "Graphic Kick", :key :graphic-kick}
     {:idx 16, :name "BC's Tube Kick", :key :bc's-tube-kick}
     {:idx 17, :name "Bellows Kick", :key :bellows-kick}
     {:idx 18, :name "Tuned Kicker C2", :key :tuned-kicker-c2}
     {:idx 19, :name "Kick Sloppy", :key :kick-sloppy}
     {:idx 20, :name "Kick Big", :key :kick-big}
     {:idx 21, :name "Kick Natural", :key :kick-natural}
     {:idx 22, :name "Kick Short", :key :kick-short}
     {:idx 23, :name "Kick Low", :key :kick-low}
     {:idx 24, :name "Kick Skin", :key :kick-skin}
     {:idx 25, :name "Big Kit Beefy Kick", :key :big-kit-beefy-kick}
     {:idx 26, :name "Big Kit Tight Kick", :key :big-kit-tight-kick}
     {:idx 27, :name "Not So Analog Kick 1", :key :not-so-analog-kick-1}
     {:idx 28, :name "Not So Analog Kick 2", :key :not-so-analog-kick-2}
     {:idx 29, :name "Funk Kick 1", :key :funk-kick-1}
     {:idx 30, :name "Funk Kick 2", :key :funk-kick-2}
     {:idx 31, :name "Funk Kick 3", :key :funk-kick-3}
     {:idx 32, :name "Kick Hop", :key :kick-hop}
     {:idx 33, :name "Dusty 808", :key :dusty-808}
     {:idx 34, :name "Dusty 808 Verb", :key :dusty-808-verb}
     {:idx 35, :name "NuNu 808", :key :nunu-808}
     {:idx 36, :name "Low End Hitter", :key :low-end-hitter}
     {:idx 37, :name "Sixty-Eight Kick", :key :sixty-eight-kick}
     {:idx 38, :name "Soft Analog Kick", :key :soft-analog-kick}
     {:idx 39, :name "Bird Kick", :key :bird-kick}]}
   {:idx 3,
    :name "Snares",
    :key :snares,
    :elements
    [{:idx 0, :name "Session Snare 1", :key :session-snare-1}
     {:idx 1, :name "Session Snare 2", :key :session-snare-2}
     {:idx 2, :name "Session Snare 3", :key :session-snare-3}
     {:idx 3, :name "Big Kit Snare", :key :big-kit-snare}
     {:idx 4, :name "Big Kit Rimshot", :key :big-kit-rimshot}
     {:idx 5, :name "Big Kit Electro Snare", :key :big-kit-electro-snare}
     {:idx 6,
      :name "Big Kit Electro Rimshot",
      :key :big-kit-electro-rimshot}
     {:idx 7, :name "Expressive Snare 1", :key :expressive-snare-1}
     {:idx 8, :name "Expressive Snare 2", :key :expressive-snare-2}
     {:idx 9, :name "Expressive Snare 3", :key :expressive-snare-3}
     {:idx 10, :name "Camembert Snare 1", :key :camembert-snare-1}
     {:idx 11, :name "Camembert Snare 2", :key :camembert-snare-2}
     {:idx 12, :name "Camembert Snare 3", :key :camembert-snare-3}
     {:idx 13, :name "Camembert Snare 4", :key :camembert-snare-4}
     {:idx 14, :name "Léa's Snare 1", :key :léa's-snare-1}
     {:idx 15, :name "Léa's Snare 2", :key :léa's-snare-2}
     {:idx 16, :name "Léa's Snare 3", :key :léa's-snare-3}
     {:idx 17, :name "Broue Snare", :key :broue-snare}
     {:idx 18, :name "OCD Snare 1", :key :ocd-snare-1}
     {:idx 19, :name "OCD Snare 2", :key :ocd-snare-2}
     {:idx 20, :name "OCD Snare 3", :key :ocd-snare-3}
     {:idx 21, :name "OCD Snare 4", :key :ocd-snare-4}
     {:idx 22, :name "OCD Snare 5", :key :ocd-snare-5}
     {:idx 23, :name "OCD Snare 6", :key :ocd-snare-6}
     {:idx 24, :name "OCD Snare 7", :key :ocd-snare-7}
     {:idx 25, :name "Jam Snare", :key :jam-snare}
     {:idx 26, :name "Funky Snare", :key :funky-snare}
     {:idx 27, :name "Snare Rio", :key :snare-rio}
     {:idx 28, :name "B Jazz Snare", :key :b-jazz-snare}
     {:idx 29, :name "Jazz Mo Snare", :key :jazz-mo-snare}
     {:idx 30, :name "Snare Jones", :key :snare-jones}
     {:idx 31, :name "Gold Snare", :key :gold-snare}
     {:idx 32, :name "SM Snare", :key :sm-snare}
     {:idx 33, :name "Woody Snare", :key :woody-snare}
     {:idx 34, :name "Snared Shot", :key :snared-shot}
     {:idx 35, :name "Snare Metal", :key :snare-metal}
     {:idx 36, :name "Dirac Snare", :key :dirac-snare}
     {:idx 37, :name "Graphic Snare", :key :graphic-snare}
     {:idx 38,
      :name "Not So Analog Snare 1",
      :key :not-so-analog-snare-1}
     {:idx 39,
      :name "Not So Analog Snare 2",
      :key :not-so-analog-snare-2}
     {:idx 40, :name "Melodic Snare", :key :melodic-snare}
     {:idx 41, :name "Snare Ringing", :key :snare-ringing}
     {:idx 42, :name "Snare Low", :key :snare-low}
     {:idx 43, :name "Snare Boomy", :key :snare-boomy}
     {:idx 44, :name "Snare Short", :key :snare-short}
     {:idx 45, :name "Snare Loose", :key :snare-loose}
     {:idx 46, :name "Snare High", :key :snare-high}
     {:idx 47, :name "Snare Tight", :key :snare-tight}
     {:idx 48, :name "Snare Cut", :key :snare-cut}
     {:idx 49, :name "Snare Bite", :key :snare-bite}
     {:idx 50, :name "Snare Deep", :key :snare-deep}
     {:idx 51, :name "Snare Snappy", :key :snare-snappy}
     {:idx 52, :name "Snare Natural", :key :snare-natural}
     {:idx 53, :name "Snare Natural Alt", :key :snare-natural-alt}
     {:idx 54,
      :name "Snare Natural Ringing",
      :key :snare-natural-ringing}
     {:idx 55, :name "Snare Plate 1", :key :snare-plate-1}
     {:idx 56, :name "Snare Plate 2", :key :snare-plate-2}
     {:idx 57, :name "Snare Plate 3", :key :snare-plate-3}
     {:idx 58, :name "Snare Works", :key :snare-works}
     {:idx 59, :name "Zombie Snare", :key :zombie-snare}
     {:idx 60, :name "Sixty-Eight Snare 1", :key :sixty-eight-snare-1}
     {:idx 61, :name "Sixty-Eight Snare 2", :key :sixty-eight-snare-2}
     {:idx 62, :name "TR Snare", :key :tr-snare}
     {:idx 63, :name "Rimshot Timbre", :key :rimshot-timbre}
     {:idx 64, :name "Rimshot Analog", :key :rimshot-analog}
     {:idx 65, :name "Rimshot Acoustic", :key :rimshot-acoustic}
     {:idx 66, :name "Rimshot  Legacy", :key :rimshot--legacy}
     {:idx 67, :name "Impact Rimshot", :key :impact-rimshot}
     {:idx 68, :name "Rimshot Gate", :key :rimshot-gate}
     {:idx 69, :name "Rimshot  Resonant", :key :rimshot--resonant}
     {:idx 70, :name "Rimshot RnB", :key :rimshot-rnb}
     {:idx 71,
      :name "Brushed Electro Snare 1",
      :key :brushed-electro-snare-1}
     {:idx 72,
      :name "Brushed Electro Snare 2",
      :key :brushed-electro-snare-2}
     {:idx 73, :name "Brushed Snare 1", :key :brushed-snare-1}
     {:idx 74, :name "Brushed Snare 1 Hold", :key :brushed-snare-1-hold}
     {:idx 75, :name "Brushed Snare 2", :key :brushed-snare-2}
     {:idx 76, :name "Brushed Snare 3", :key :brushed-snare-3}
     {:idx 77, :name "Brush and Clap", :key :brush-and-clap}
     {:idx 78, :name "Tambour Snare", :key :tambour-snare}
     {:idx 79, :name "King of Snare 1", :key :king-of-snare-1}
     {:idx 80, :name "King of Snare 2", :key :king-of-snare-2}
     {:idx 81, :name "King of Snare 3", :key :king-of-snare-3}
     {:idx 82, :name "King of Snare 4", :key :king-of-snare-4}
     {:idx 83, :name "Big Kit Side Stick", :key :big-kit-side-stick}]}
   {:idx 4,
    :name "Toms",
    :key :toms,
    :elements
    [{:idx 0, :name "Clark Tom High 1", :key :clark-tom-high-1}
     {:idx 1, :name "Clark Tom Middle 1", :key :clark-tom-middle-1}
     {:idx 2, :name "Clark Tom Low 1", :key :clark-tom-low-1}
     {:idx 3, :name "Clark Tom High 2", :key :clark-tom-high-2}
     {:idx 4, :name "Clark Tom Middle 2", :key :clark-tom-middle-2}
     {:idx 5, :name "Clark Tom Low 2", :key :clark-tom-low-2}
     {:idx 6, :name "Léa's High Tom", :key :léa's-high-tom}
     {:idx 7, :name "Léa's Middle Tom", :key :léa's-middle-tom}
     {:idx 8, :name "Léa's Low Tom", :key :léa's-low-tom}
     {:idx 9, :name "High Tom Jazz", :key :high-tom-jazz}
     {:idx 10, :name "Med Tom Jazz", :key :med-tom-jazz}
     {:idx 11, :name "Low Tom Jazz", :key :low-tom-jazz}
     {:idx 12, :name "High Tom Trax", :key :high-tom-trax}
     {:idx 13, :name "Med Tom Trax", :key :med-tom-trax}
     {:idx 14, :name "Low Tom Trax", :key :low-tom-trax}
     {:idx 15, :name "High Tom Round", :key :high-tom-round}
     {:idx 16, :name "Med Tom Round", :key :med-tom-round}
     {:idx 17, :name "Low Tom Round", :key :low-tom-round}
     {:idx 18, :name "High Tom Natural", :key :high-tom-natural}
     {:idx 19, :name "Med Tom Natural", :key :med-tom-natural}
     {:idx 20, :name "Low Tom Natural", :key :low-tom-natural}
     {:idx 21, :name "Big Kit High Tom", :key :big-kit-high-tom}
     {:idx 22, :name "Big Kit Mid Tom", :key :big-kit-mid-tom}
     {:idx 23, :name "Big Kit Floor Tom", :key :big-kit-floor-tom}
     {:idx 24, :name "Not So Analog Tom", :key :not-so-analog-tom}
     {:idx 25, :name "Tom Tribe", :key :tom-tribe}
     {:idx 26, :name "Cinematic Tom", :key :cinematic-tom}]}
   {:idx 5,
    :name "Hi-Hats",
    :key :hi-hats,
    :elements
    [{:idx 0, :name "Hi-Hat Trick 1", :key :hi-hat-trick-1}
     {:idx 1, :name "Hi-Hat Trick 2", :key :hi-hat-trick-2}
     {:idx 2, :name "Hi-Hat Trick 3", :key :hi-hat-trick-3}
     {:idx 3, :name "Hi-Hat Trick 4", :key :hi-hat-trick-4}
     {:idx 4, :name "Hi-Hat Trick 5", :key :hi-hat-trick-5}
     {:idx 5, :name "Hi-Hat Trick 6", :key :hi-hat-trick-6}
     {:idx 6, :name "Hi-Hat Trick 7", :key :hi-hat-trick-7}
     {:idx 7, :name "Hi-Hat Trick 8", :key :hi-hat-trick-8}
     {:idx 8, :name "Hi-Hat Trick 9", :key :hi-hat-trick-9}
     {:idx 9, :name "Big Kit Closed Hi-Hat", :key :big-kit-closed-hi-hat}
     {:idx 10, :name "Big Kit Open Hi-Hat", :key :big-kit-open-hi-hat}
     {:idx 11,
      :name "Big Kit Open Hi-Hat Gated",
      :key :big-kit-open-hi-hat-gated}
     {:idx 12, :name "Big Kit Pedal Hi-Hat", :key :big-kit-pedal-hi-hat}
     {:idx 13,
      :name "Big Kit Splashable Hi-Hat",
      :key :big-kit-splashable-hi-hat}
     {:idx 14, :name "Sharp Hi-Hat", :key :sharp-hi-hat}
     {:idx 15, :name "Hi-Hat Basic", :key :hi-hat-basic}
     {:idx 16, :name "Hi-Hat Trax 1", :key :hi-hat-trax-1}
     {:idx 17, :name "Hi-Hat Trax 2", :key :hi-hat-trax-2}
     {:idx 18, :name "Hi-Hat Trax 3", :key :hi-hat-trax-3}
     {:idx 19, :name "Hi-Hat Foot", :key :hi-hat-foot}
     {:idx 20, :name "Hi-Hat Natural 1", :key :hi-hat-natural-1}
     {:idx 21, :name "Hi-Hat Natural 2", :key :hi-hat-natural-2}
     {:idx 22, :name "Chick'n'sizzle Hats", :key :chick'n'sizzle-hats}
     {:idx 23, :name "Middle Hi-Hat Gated", :key :middle-hi-hat-gated}
     {:idx 24, :name "1940 Hi-Hat Gated", :key :1940-hi-hat-gated}]}
   {:idx 6,
    :name "Cymbals",
    :key :cymbals,
    :elements
    [{:idx 0, :name "Big Kit Crash 1", :key :big-kit-crash-1}
     {:idx 1, :name "Big Kit Crash 2", :key :big-kit-crash-2}
     {:idx 2, :name "Big Kit Crash 3", :key :big-kit-crash-3}
     {:idx 3, :name "Big Kit Splash", :key :big-kit-splash}
     {:idx 4, :name "Big Kit Ride", :key :big-kit-ride}
     {:idx 5,
      :name "Big Kit Ride Velo Bell",
      :key :big-kit-ride-velo-bell}
     {:idx 6, :name "Short Ride", :key :short-ride}
     {:idx 7, :name "Rigid Ride", :key :rigid-ride}
     {:idx 8, :name "Dirty Ride", :key :dirty-ride}
     {:idx 9, :name "Small Gong Ride", :key :small-gong-ride}
     {:idx 10, :name "Medium Gong Ride", :key :medium-gong-ride}
     {:idx 11, :name "Large Gong Ride", :key :large-gong-ride}
     {:idx 12, :name "Cymbatic 1", :key :cymbatic-1}
     {:idx 13, :name "Cymbatic 2", :key :cymbatic-2}
     {:idx 14, :name "Cymbatic 3", :key :cymbatic-3}
     {:idx 15, :name "Cymbatic 4", :key :cymbatic-4}
     {:idx 16, :name "Cymbatic 4 Dark", :key :cymbatic-4-dark}
     {:idx 17, :name "Cymbatic 4 Hollow", :key :cymbatic-4-hollow}
     {:idx 18, :name "Cymbatic 5", :key :cymbatic-5}
     {:idx 19, :name "Cymbatic 6", :key :cymbatic-6}
     {:idx 20, :name "Cymbatic 6 Bright", :key :cymbatic-6-bright}
     {:idx 21,
      :name "Big Kit Chinese Cymbal",
      :key :big-kit-chinese-cymbal}
     {:idx 22, :name "Chinese Cymbal", :key :chinese-cymbal}
     {:idx 23, :name "Gongy Plate", :key :gongy-plate}
     {:idx 24, :name "Tiger Gong", :key :tiger-gong}
     {:idx 25, :name "Bright Gong", :key :bright-gong}
     {:idx 26,
      :name "Ringing Metal Plates",
      :key :ringing-metal-plates}]}
   {:idx 7,
    :name "Chromakits",
    :key :chromakits,
    :elements
    [{:idx 0, :name "Snare Kit", :key :snare-kit}
     {:idx 1, :name "Destructo Kit", :key :destructo-kit}
     {:idx 2, :name "Clap Kit Four", :key :clap-kit-four}
     {:idx 3, :name "Clap Kit Two", :key :clap-kit-two}
     {:idx 4, :name "Heavy Duty Kit 1", :key :heavy-duty-kit-1}
     {:idx 5, :name "Heavy Duty Kit 2", :key :heavy-duty-kit-2}
     {:idx 6, :name "Electroacoustic Kit 1", :key :electroacoustic-kit-1}
     {:idx 7, :name "Electroacoustic Kit 2", :key :electroacoustic-kit-2}
     {:idx 8, :name "Electroacoustic Kit 3", :key :electroacoustic-kit-3}
     {:idx 9, :name "Electroacoustic Kit 4", :key :electroacoustic-kit-4}
     {:idx 10, :name "Snare Plate Kit", :key :snare-plate-kit}
     {:idx 11, :name "Analog Kit 1", :key :analog-kit-1}
     {:idx 12, :name "Analog Kit 2", :key :analog-kit-2}
     {:idx 13, :name "Street Corner Kit", :key :street-corner-kit}
     {:idx 14,
      :name "Street Corner Kit Gated",
      :key :street-corner-kit-gated}
     {:idx 15, :name "Electrotubular Kit", :key :electrotubular-kit}
     {:idx 16, :name "Ceramic Pan Kit", :key :ceramic-pan-kit}
     {:idx 17, :name "Manual Percu Kit", :key :manual-percu-kit}
     {:idx 18, :name "Huge Hall Kit", :key :huge-hall-kit}
     {:idx 19, :name "Tonal Kit", :key :tonal-kit}
     {:idx 20, :name "Mangled Menu Kit", :key :mangled-menu-kit}
     {:idx 21, :name "Marimba Kit", :key :marimba-kit}
     {:idx 22, :name "Backyard Kit", :key :backyard-kit}
     {:idx 23, :name "Bendy Kit", :key :bendy-kit}
     {:idx 24, :name "Cystom Tom Kit", :key :cystom-tom-kit}]}
   {:idx 8,
    :name "Chimes and Bells",
    :key :chimes-and-bells,
    :elements
    [{:idx 0, :name "Home Alone", :key :home-alone}
     {:idx 1, :name "Smooth Carillon", :key :smooth-carillon}
     {:idx 2, :name "Barbell", :key :barbell}
     {:idx 3, :name "Amimba", :key :amimba}
     {:idx 4, :name "Metallica", :key :metallica}
     {:idx 5, :name "Glasses", :key :glasses}
     {:idx 6, :name "Chimophone", :key :chimophone}
     {:idx 7, :name "Chorused Glock", :key :chorused-glock}
     {:idx 8, :name "Vapourous Beam", :key :vapourous-beam}
     {:idx 9, :name "Clockworks", :key :clockworks}
     {:idx 10,
      :name "For Whom the Bell Tolls",
      :key :for-whom-the-bell-tolls}
     {:idx 11, :name "Glass Automaton", :key :glass-automaton}
     {:idx 12, :name "Calling Sticks", :key :calling-sticks}
     {:idx 13, :name "Beux Dreamer", :key :beux-dreamer}
     {:idx 14, :name "Mr Beam", :key :mr-beam}
     {:idx 15, :name "Bell Clusters", :key :bell-clusters}
     {:idx 16, :name "Chorused Plates", :key :chorused-plates}
     {:idx 17, :name "Carriage Clock", :key :carriage-clock}
     {:idx 18, :name "Take Me to Church", :key :take-me-to-church}
     {:idx 19, :name "Chromatic Bells", :key :chromatic-bells}
     {:idx 20, :name "Hand Bells", :key :hand-bells}
     {:idx 21, :name "Platubular Bells", :key :platubular-bells}
     {:idx 22, :name "Metalworks 1", :key :metalworks-1}
     {:idx 23, :name "Metalworks 2", :key :metalworks-2}
     {:idx 24,
      :name "Particles of Tranquility",
      :key :particles-of-tranquility}
     {:idx 25, :name "Ballerina Twirl", :key :ballerina-twirl}
     {:idx 26,
      :name "Auto Chimes Glissando",
      :key :auto-chimes-glissando}
     {:idx 27, :name "Wind Chimes", :key :wind-chimes}
     {:idx 28, :name "Large Bell", :key :large-bell}]}
   {:idx 9,
    :name "Plucked Strings",
    :key :plucked-strings,
    :elements
    [{:idx 0, :name "Beauty in Numbers", :key :beauty-in-numbers}
     {:idx 1,
      :name "Electric Contemplation",
      :key :electric-contemplation}
     {:idx 2, :name "Sleepless Nights", :key :sleepless-nights}
     {:idx 3, :name "Double-String Flange", :key :double-string-flange}
     {:idx 4, :name "Bell Guitar", :key :bell-guitar}
     {:idx 5, :name "Purple Nights", :key :purple-nights}
     {:idx 6, :name "Chorused Driven", :key :chorused-driven}
     {:idx 7, :name "Lead GT", :key :lead-gt}
     {:idx 8, :name "Howling Electric", :key :howling-electric}
     {:idx 9, :name "Electric Acoustic", :key :electric-acoustic}
     {:idx 10, :name "Pizzy", :key :pizzy}
     {:idx 11, :name "Harp Tube", :key :harp-tube}
     {:idx 12, :name "Sweet Harmonics", :key :sweet-harmonics}
     {:idx 13, :name "Soft Harp", :key :soft-harp}
     {:idx 14, :name "Fifth Harp", :key :fifth-harp}
     {:idx 15, :name "Chromatronics", :key :chromatronics}
     {:idx 16, :name "Mystic Space", :key :mystic-space}
     {:idx 17, :name "Fall Arpeggios", :key :fall-arpeggios}
     {:idx 18, :name "Tension", :key :tension}
     {:idx 19, :name "Koto Ancestor", :key :koto-ancestor}
     {:idx 20, :name "Sun Ghanelja", :key :sun-ghanelja}
     {:idx 21, :name "Moon Ghanelja", :key :moon-ghanelja}
     {:idx 22, :name "Star Ghanelja", :key :star-ghanelja}
     {:idx 23, :name "Eastern Strings", :key :eastern-strings}
     {:idx 24, :name "Two Tars", :key :two-tars}
     {:idx 25, :name "Saburau", :key :saburau}
     {:idx 26, :name "Natural Bamako", :key :natural-bamako}
     {:idx 27, :name "Dulcimator", :key :dulcimator}
     {:idx 28, :name "Inverted Dulcimer", :key :inverted-dulcimer}
     {:idx 29,
      :name "Whole Tone Dulcimer from Space",
      :key :whole-tone-dulcimer-from-space}
     {:idx 30, :name "Hammered Cymbalom", :key :hammered-cymbalom}
     {:idx 31, :name "Plucked Cymbalom", :key :plucked-cymbalom}
     {:idx 32, :name "Wah Wah Cymbalom", :key :wah-wah-cymbalom}
     {:idx 33, :name "Another World", :key :another-world}]}
   {:idx 10,
    :name "Basses",
    :key :basses,
    :elements
    [{:idx 0, :name "Slappy", :key :slappy}
     {:idx 1, :name "Woody E. Bass 1", :key :woody-e.-bass-1}
     {:idx 2, :name "Woody E. Bass 2", :key :woody-e.-bass-2}
     {:idx 3, :name "Woody E. Bass 3", :key :woody-e.-bass-3}
     {:idx 4, :name "Friday", :key :friday}
     {:idx 5, :name "Godsend", :key :godsend}
     {:idx 6, :name "Bold Bass", :key :bold-bass}
     {:idx 7, :name "Boa Bass 1", :key :boa-bass-1}
     {:idx 8, :name "Boa Bass 2", :key :boa-bass-2}
     {:idx 9, :name "Plump Bass", :key :plump-bass}
     {:idx 10, :name "Elliptic Bass", :key :elliptic-bass}
     {:idx 11, :name "Hard Lacquer Bass", :key :hard-lacquer-bass}
     {:idx 12, :name "Bin Bass", :key :bin-bass}
     {:idx 13, :name "Soft 4 String", :key :soft-4-string}
     {:idx 14, :name "Tube Bass", :key :tube-bass}
     {:idx 15, :name "Euro", :key :euro}
     {:idx 16, :name "Dark Bass", :key :dark-bass}
     {:idx 17, :name "Ladybird Bass", :key :ladybird-bass}
     {:idx 18, :name "Ze Funky Bass", :key :ze-funky-bass}
     {:idx 19, :name "Comando Bass", :key :comando-bass}
     {:idx 20, :name "Velo-Slap Bass", :key :velo-slap-bass}
     {:idx 21, :name "Flanging Bass", :key :flanging-bass}
     {:idx 22, :name "Growling B", :key :growling-b}
     {:idx 23, :name "'Brane Bass Dark", :key :'brane-bass-dark}
     {:idx 24, :name "'Brane Bass Bright", :key :'brane-bass-bright}
     {:idx 25, :name "Mouthy", :key :mouthy}
     {:idx 26, :name "Moody Slap Bass", :key :moody-slap-bass}
     {:idx 27, :name "Electronic Bass", :key :electronic-bass}
     {:idx 28, :name "UFO  Bass", :key :ufo--bass}
     {:idx 29, :name "Chifty Tuby Bass", :key :chifty-tuby-bass}
     {:idx 30, :name "Welcome to the House", :key :welcome-to-the-house}
     {:idx 31, :name "Trance at NAMM", :key :trance-at-namm}
     {:idx 32, :name "Vacationer Pluck", :key :vacationer-pluck}]}
   {:idx 11,
    :name "Keys",
    :key :keys,
    :elements
    [{:idx 0, :name "Ultranet Full", :key :ultranet-full}
     {:idx 1, :name "Ultranet Full Body", :key :ultranet-full-body}
     {:idx 2, :name "Ultranet Full Funk", :key :ultranet-full-funk}
     {:idx 3, :name "Ultranet Full Short", :key :ultranet-full-short}
     {:idx 4, :name "Ultranet Mellow", :key :ultranet-mellow}
     {:idx 5, :name "Ultranet Mellow Not", :key :ultranet-mellow-not}
     {:idx 6, :name "Ultranet Bright", :key :ultranet-bright}
     {:idx 7,
      :name "Ultranet Bright Octaver",
      :key :ultranet-bright-octaver}
     {:idx 8, :name "Ultranet Bass Snap", :key :ultranet-bass-snap}
     {:idx 9, :name "Ultranet Bright Funk", :key :ultranet-bright-funk}
     {:idx 10, :name "Chromafunk", :key :chromafunk}
     {:idx 11, :name "Insidious", :key :insidious}
     {:idx 12, :name "Bright Thingy", :key :bright-thingy}
     {:idx 13, :name "So Very Funky", :key :so-very-funky}
     {:idx 14, :name "Ah Clav", :key :ah-clav}
     {:idx 15, :name "Dry d-six", :key :dry-d-six}
     {:idx 16, :name "Smooth Electric", :key :smooth-electric}
     {:idx 17, :name "Lounge Piano", :key :lounge-piano}
     {:idx 18,
      :name "The Darker Side of EP",
      :key :the-darker-side-of-ep}
     {:idx 19, :name "Phased EP", :key :phased-ep}
     {:idx 20, :name "Smooth Bepiano", :key :smooth-bepiano}
     {:idx 21, :name "The Heaven", :key :the-heaven}
     {:idx 22, :name "Agonizing EP", :key :agonizing-ep}
     {:idx 23, :name "Toy Percussion", :key :toy-percussion}
     {:idx 24, :name "Toy Piano", :key :toy-piano}
     {:idx 25, :name "Celesta", :key :celesta}]}
   {:idx 12,
    :name "Strings and Pads",
    :key :strings-and-pads,
    :elements
    [{:idx 0, :name "Frozen in Time", :key :frozen-in-time}
     {:idx 1, :name "Wutherings", :key :wutherings}
     {:idx 2, :name "Materializing", :key :materializing}
     {:idx 3, :name "Scintillator Strings", :key :scintillator-strings}
     {:idx 4, :name "Growling Chroma", :key :growling-chroma}
     {:idx 5, :name "Guitar Pad", :key :guitar-pad}
     {:idx 6, :name "Plucked Pad", :key :plucked-pad}
     {:idx 7, :name "Bathtub Pad", :key :bathtub-pad}
     {:idx 8, :name "Uberstreich", :key :uberstreich}
     {:idx 9,
      :name "Interesting Hollow Effect",
      :key :interesting-hollow-effect}
     {:idx 10, :name "Mellowtron", :key :mellowtron}
     {:idx 11, :name "'Tron Strings", :key :'tron-strings}
     {:idx 12, :name "'Tron Octave Strings", :key :'tron-octave-strings}
     {:idx 13, :name "Sautillé 1", :key :sautillé-1}
     {:idx 14, :name "Sautillé 2", :key :sautillé-2}
     {:idx 15, :name "Tremmy Lemmy Pad", :key :tremmy-lemmy-pad}
     {:idx 16, :name "Bowed", :key :bowed}
     {:idx 17, :name "Ensemble 1", :key :ensemble-1}
     {:idx 18, :name "Ensemble 2", :key :ensemble-2}
     {:idx 19, :name "Decaying Ensemble", :key :decaying-ensemble}]}
   {:idx 13,
    :name "Synths",
    :key :synths,
    :elements
    [{:idx 0, :name "Vintage Tubes 1", :key :vintage-tubes-1}
     {:idx 1, :name "Vintage Tubes 2", :key :vintage-tubes-2}
     {:idx 2, :name "Sample & Hold & Noise", :key :sample-&-hold-&-noise}
     {:idx 3, :name "Nirvana", :key :nirvana}
     {:idx 4, :name "Chiff Delay", :key :chiff-delay}
     {:idx 5, :name "Bouncy Chiff", :key :bouncy-chiff}
     {:idx 6, :name "Timber", :key :timber}
     {:idx 7, :name "Tube Love", :key :tube-love}
     {:idx 8, :name "Tranzy Pluck", :key :tranzy-pluck}
     {:idx 9, :name "Club Tubes", :key :club-tubes}
     {:idx 10, :name "Polyester", :key :polyester}
     {:idx 11, :name "Winter's Bone", :key :winter's-bone}
     {:idx 12, :name "Is It a Saw?", :key :is-it-a-saw?}
     {:idx 13, :name "Magical Tubes", :key :magical-tubes}
     {:idx 14, :name "Harmonix Lead", :key :harmonix-lead}
     {:idx 15,
      :name "Harmonix Lead Crushed",
      :key :harmonix-lead-crushed}
     {:idx 16, :name "Ambient Pizz", :key :ambient-pizz}
     {:idx 17, :name "Membrains", :key :membrains}
     {:idx 18, :name "Plucked Tube", :key :plucked-tube}
     {:idx 19, :name "DMU Pluck", :key :dmu-pluck}
     {:idx 20, :name "Foreign Shores", :key :foreign-shores}
     {:idx 21, :name "Belly Pluck", :key :belly-pluck}
     {:idx 22, :name "SID Gamer Lead", :key :sid-gamer-lead}
     {:idx 23, :name "Chord 1", :key :chord-1}
     {:idx 24, :name "Chord 2", :key :chord-2}]}
   {:idx 14,
    :name "Organs and Pipes",
    :key :organs-and-pipes,
    :elements
    [{:idx 0, :name "Flute 8' + 4'", :key :flute-8'-+-4'}
     {:idx 1, :name "Big Pipes", :key :big-pipes}
     {:idx 2, :name "The Circus Is in Town", :key :the-circus-is-in-town}
     {:idx 3, :name "Old Rusty", :key :old-rusty}
     {:idx 4, :name "Organ Attack", :key :organ-attack}
     {:idx 5,
      :name "Insane in the Membrane",
      :key :insane-in-the-membrane}
     {:idx 6, :name "North Wind Organ", :key :north-wind-organ}
     {:idx 7, :name "Steelpan-Organ Hybrid", :key :steelpan-organ-hybrid}
     {:idx 8, :name "'Tron Flute", :key :'tron-flute}
     {:idx 9, :name "Aerophone", :key :aerophone}
     {:idx 10, :name "Machu Flute", :key :machu-flute}]}
   {:idx 15,
    :name "Soundscapes",
    :key :soundscapes,
    :elements
    [{:idx 0, :name "Carpetry", :key :carpetry}
     {:idx 1, :name "Eery", :key :eery}
     {:idx 2, :name "Esoteric", :key :esoteric}
     {:idx 3, :name "Ethereal Sweep", :key :ethereal-sweep}
     {:idx 4, :name "Grain Pad", :key :grain-pad}
     {:idx 5, :name "Outer Reaches", :key :outer-reaches}
     {:idx 6, :name "Chroma Realms", :key :chroma-realms}
     {:idx 7, :name "Fractal Pad", :key :fractal-pad}
     {:idx 8, :name "Horror Harp", :key :horror-harp}
     {:idx 9, :name "Underground Wind", :key :underground-wind}
     {:idx 10, :name "The Deep", :key :the-deep}
     {:idx 11, :name "Scary Movie", :key :scary-movie}
     {:idx 12, :name "Solar Spring", :key :solar-spring}
     {:idx 13, :name "Storm Cloud", :key :storm-cloud}
     {:idx 14, :name "Bathing Pools", :key :bathing-pools}
     {:idx 15, :name "Cami Night", :key :cami-night}
     {:idx 16, :name "Stretching Glass", :key :stretching-glass}
     {:idx 17, :name "Uncertainty", :key :uncertainty}
     {:idx 18, :name "Raining Tubes", :key :raining-tubes}
     {:idx 19, :name "Rosin Pipeworks", :key :rosin-pipeworks}
     {:idx 20, :name "Blow Holes", :key :blow-holes}
     {:idx 21, :name "Leakage", :key :leakage}
     {:idx 22, :name "Rattler", :key :rattler}
     {:idx 23,
      :name "Battles of Dreamworld",
      :key :battles-of-dreamworld}
     {:idx 24, :name "Particle Romance", :key :particle-romance}
     {:idx 25, :name "Light Saber", :key :light-saber}
     {:idx 26, :name "Digeridrone", :key :digeridrone}
     {:idx 27, :name "Heavy Drone", :key :heavy-drone}
     {:idx 28, :name "Low Drone", :key :low-drone}
     {:idx 29, :name "Mysterio", :key :mysterio}
     {:idx 30, :name "Scratch 'n Sniff", :key :scratch-'n-sniff}
     {:idx 31,
      :name "Scrapers from the Deep",
      :key :scrapers-from-the-deep}
     {:idx 32, :name "Dark Insight", :key :dark-insight}
     {:idx 33,
      :name "Mallet in a Metal Bowl",
      :key :mallet-in-a-metal-bowl}
     {:idx 34, :name "Chime-o-matic", :key :chime-o-matic}
     {:idx 35, :name "Ringing Beam", :key :ringing-beam}
     {:idx 36, :name "Deep Well", :key :deep-well}
     {:idx 37, :name "Chorused Plates", :key :chorused-plates}
     {:idx 38, :name "Meditative Crash", :key :meditative-crash}
     {:idx 39, :name "Stockhausen Mallet", :key :stockhausen-mallet}
     {:idx 40, :name "Width", :key :width}]}
   {:idx 16,
    :name "Effects",
    :key :effects,
    :elements
    [{:idx 0, :name "Submariner", :key :submariner}
     {:idx 1, :name "Shallow Halo", :key :shallow-halo}
     {:idx 2, :name "Baudelaire", :key :baudelaire}
     {:idx 3, :name "DBZ Drama", :key :dbz-drama}
     {:idx 4, :name "Gong of Doom", :key :gong-of-doom}
     {:idx 5, :name "Hollow Plates", :key :hollow-plates}
     {:idx 6, :name "Ultimate Motor Sound", :key :ultimate-motor-sound}
     {:idx 7, :name "Safari", :key :safari}
     {:idx 8, :name "Water Filled Pan", :key :water-filled-pan}
     {:idx 9, :name "Falling Gongs", :key :falling-gongs}
     {:idx 10,
      :name "Falling Metal Percussion",
      :key :falling-metal-percussion}
     {:idx 11, :name "Wander Lust", :key :wander-lust}
     {:idx 12, :name "Harmonic Phasing", :key :harmonic-phasing}
     {:idx 13, :name "Flanged Plates", :key :flanged-plates}
     {:idx 14,
      :name "Strange Little Things",
      :key :strange-little-things}
     {:idx 15, :name "Crackle OK", :key :crackle-ok}
     {:idx 16, :name "Windy 1", :key :windy-1}
     {:idx 17, :name "Windy 2", :key :windy-2}
     {:idx 18, :name "Brass Cup", :key :brass-cup}
     {:idx 19, :name "Canteen", :key :canteen}
     {:idx 20, :name "Ever", :key :ever}
     {:idx 21, :name "Stairway", :key :stairway}
     {:idx 22, :name "Waterbirds", :key :waterbirds}
     {:idx 23, :name "Birds Eye View", :key :birds-eye-view}
     {:idx 24, :name "Warfare", :key :warfare}
     {:idx 25, :name "Cosmic E Flat", :key :cosmic-e-flat}
     {:idx 26, :name "School's Out", :key :school's-out}
     {:idx 27, :name "Horror Phone", :key :horror-phone}
     {:idx 28, :name "Water Blocks", :key :water-blocks}
     {:idx 29, :name "Cave Explorers", :key :cave-explorers}
     {:idx 30, :name "Rocket Science", :key :rocket-science}
     {:idx 31, :name "Chuck's Toy Piano", :key :chuck's-toy-piano}
     {:idx 32, :name "Cinematic Four", :key :cinematic-four}]}
   {:idx 17,
    :name "Arpeggiators",
    :key :arpeggiators,
    :elements
    [{:idx 0, :name "Magical Bell Arp", :key :magical-bell-arp}
     {:idx 1, :name "Floating On Waves Arp", :key :floating-on-waves-arp}
     {:idx 2, :name "Aquarius Arp", :key :aquarius-arp}
     {:idx 3, :name "Harpy Arpy", :key :harpy-arpy}
     {:idx 4, :name "AK Analog Arp", :key :ak-analog-arp}
     {:idx 5, :name "Nost Mallet Arp", :key :nost-mallet-arp}
     {:idx 6, :name "Slightly Analog Arp", :key :slightly-analog-arp}
     {:idx 7,
      :name "Cinematic Guitar Player",
      :key :cinematic-guitar-player}
     {:idx 8, :name "Scene", :key :scene}
     {:idx 9,
      :name "I Would Go to That Party",
      :key :i-would-go-to-that-party}
     {:idx 10, :name "Noce Psy Trance Bass", :key :noce-psy-trance-bass}
     {:idx 11, :name "Psy 5ths", :key :psy-5ths}
     {:idx 12, :name "Tropical Highlights", :key :tropical-highlights}
     {:idx 13, :name "Song Patch", :key :song-patch}
     {:idx 14, :name "Reports of Vice City", :key :reports-of-vice-city}
     {:idx 15, :name "Octave Magician", :key :octave-magician}
     {:idx 16, :name "Haas Chords", :key :haas-chords}
     {:idx 17, :name "Funky Guitar", :key :funky-guitar}
     {:idx 18, :name "Trance Master", :key :trance-master}
     {:idx 19, :name "Dance of Ghosts", :key :dance-of-ghosts}
     {:idx 20, :name "Vortex of Bells", :key :vortex-of-bells}
     {:idx 21, :name "Start the Funk", :key :start-the-funk}
     {:idx 22, :name "Meditational Bells", :key :meditational-bells}
     {:idx 23, :name "Enchanted Forest", :key :enchanted-forest}
     {:idx 24, :name "Deep Space Marimba", :key :deep-space-marimba}
     {:idx 25, :name "Dream Tube", :key :dream-tube}
     {:idx 26, :name "Tri Marimba", :key :tri-marimba}
     {:idx 27, :name "Boingo Oingo", :key :boingo-oingo}
     {:idx 28, :name "Airy Dance", :key :airy-dance}
     {:idx 29, :name "Sunny Vibe", :key :sunny-vibe}
     {:idx 30, :name "Spook Xylo", :key :spook-xylo}
     {:idx 31, :name "Castle Stairs", :key :castle-stairs}
     {:idx 32, :name "Wild Thoughts", :key :wild-thoughts}
     {:idx 33, :name "80's Dance", :key :80's-dance}
     {:idx 34, :name "Carl Sagan", :key :carl-sagan}
     {:idx 35, :name "A New World", :key :a-new-world}
     {:idx 36, :name "Glass Onion", :key :glass-onion}
     {:idx 37, :name "Skyboarding", :key :skyboarding}
     {:idx 38, :name "Exploring Caves", :key :exploring-caves}]}
   {:idx 18,
    :name "Arpeggiated Percussions",
    :key :arpeggiated-percussions,
    :elements
    [{:idx 0, :name "Disturbing", :key :disturbing}
     {:idx 1, :name "Weather", :key :weather}
     {:idx 2, :name "Plate Grooves", :key :plate-grooves}
     {:idx 3, :name "Tech Crystals", :key :tech-crystals}
     {:idx 4, :name "Auto Conga", :key :auto-conga}
     {:idx 5, :name "Extreme Six", :key :extreme-six}
     {:idx 6, :name "LFO Pandeiro", :key :lfo-pandeiro}
     {:idx 7, :name "House or Not", :key :house-or-not}
     {:idx 8, :name "Cinematic Drums 1", :key :cinematic-drums-1}
     {:idx 9, :name "Cinematic Drums 2", :key :cinematic-drums-2}
     {:idx 10, :name "Cool Tech Song", :key :cool-tech-song}
     {:idx 11, :name "Jam on Dishes", :key :jam-on-dishes}
     {:idx 12, :name "Hi-Hat Comp", :key :hi-hat-comp}
     {:idx 13, :name "House of Bells", :key :house-of-bells}
     {:idx 14, :name "This Is Just Crazy", :key :this-is-just-crazy}
     {:idx 15, :name "Substantial", :key :substantial}
     {:idx 16, :name "Melodic Snare", :key :melodic-snare}
     {:idx 17, :name "16th Enhancer", :key :16th-enhancer}
     {:idx 18, :name "Mental Journey", :key :mental-journey}
     {:idx 19, :name "Spicy Glitch", :key :spicy-glitch}
     {:idx 20, :name "Scratchy Beat", :key :scratchy-beat}
     {:idx 21, :name "Jazz Cymbal", :key :jazz-cymbal}
     {:idx 22, :name "Varigroove", :key :varigroove}
     {:idx 23, :name "Tech Hit Beat", :key :tech-hit-beat}
     {:idx 24, :name "Water Conga Beat", :key :water-conga-beat}
     {:idx 25, :name "Sultan Is Here", :key :sultan-is-here}
     {:idx 26, :name "A New Bongo", :key :a-new-bongo}
     {:idx 27, :name "High Bongo", :key :high-bongo}
     {:idx 28, :name "Dizzy Yet?", :key :dizzy-yet?}
     {:idx 29, :name "Crazy Combos", :key :crazy-combos}
     {:idx 30, :name "Singing Drums", :key :singing-drums}
     {:idx 31, :name "Mystery Drum", :key :mystery-drum}
     {:idx 32, :name "Perolatedfun", :key :perolatedfun}
     {:idx 33, :name "Airy Trip Perc", :key :airy-trip-perc}]}
   {:idx 19,
    :name "Basics",
    :key :basics,
    :elements
    [{:idx 0, :name "Initialization", :key :initialization}
     {:idx 1, :name "Vibraphone", :key :vibraphone}
     {:idx 2, :name "Hand Percussion", :key :hand-percussion}
     {:idx 3, :name "Kick", :key :kick}
     {:idx 4, :name "Snare", :key :snare}
     {:idx 5, :name "Hi-Hat", :key :hi-hat}
     {:idx 6, :name "Bass", :key :bass}
     {:idx 7, :name "Bowed String", :key :bowed-string}
     {:idx 8, :name "Synth", :key :synth}
     {:idx 9, :name "Ambient", :key :ambient}]}])
