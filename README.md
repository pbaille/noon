![CI](https://img.shields.io/github/actions/workflow/status/pbaille/noon/test.yml?style=flat-square&branch=main)
[![cljdoc badge](https://cljdoc.org/badge/org.clojars.pbaille/noon)](https://cljdoc.org/d/org.clojars.pbaille/noon)
[![Clojars Project](https://img.shields.io/clojars/v/com.pbaille/noon.svg?include_prereleases)](https://clojars.org/com.pbaille/noon)

# Noon


Compose and play **MIDI** music.

> **ⓘ**  
> **NOON** *translates to* **MIDI** *in french*.

Try it [online!](https://pbaille.github.io/noon/)

https://github.com/user-attachments/assets/324a2e7c-7d13-4cc5-aedb-141f8a8347e9

## Usage 

Add the following dependency to your `deps.edn`:

``` clojure
com.pbaille/noon {:git/url "https://github.com/pbaille/noon.git"
                  :sha "a0b34e5d19fd6d1f34d6e8bcab17555d1752820e"}
```

Then you should be able to play something like this:

``` clojure
(require '[noon.eval :refer [play]])

(play (par s0 s1 s2))
```

Please refer to the [documentation](https://pbaille.github.io/noon/) for more details.
