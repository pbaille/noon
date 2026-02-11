# Noon

A Clojure library for composing and playing MIDI music ("noon" means "MIDI" in French).

## Tech Stack
- Language: Clojure/ClojureScript (cross-platform `.cljc` files)
- Build: deps.edn with Babashka tasks
- Client: shadow-cljs, UIx (React), stylefy
- Testing: cognitect test-runner (CLJ), cljs-test-runner (CLJS)

## Project Structure
```
src/noon/
├── score.cljc       # Core: scores are sets of event maps
├── events.cljc      # Event representation and manipulation
├── updates.cljc     # Score transformation functions
├── eval.cljc        # Evaluation and playback (play function)
├── harmonic_context.cljc  # Harmony/scale handling
├── lib/             # High-level composition helpers (harmony, melody, rhythm)
├── utils/           # General utilities
└── output/          # MIDI output (JVM) and Web Audio (browser)

client/              # Web UI (ClojureScript)
doc/                 # Documentation in org-mode format
test/                # Mirrors src/ structure
```

## Development Commands
```bash
# Build doc tests (REQUIRED before testing)
bb build-doc-tests

# Run tests
clojure -X:test          # CLJ tests
clojure -M:cljs-test     # CLJS tests

# Client development
npx shadow-cljs watch client

# Linting
clj-kondo --lint src test client
```

## Key Gotchas
- **Org-based docs**: Documentation lives in `doc/noon.org`. Tests are generated from code blocks in these org files via `bb build-doc-tests`. Always regenerate before running tests.
- **Cross-platform**: Most files are `.cljc`. Use reader conditionals (`#?(:clj ... :cljs ...)`) when platform-specific behavior is needed.

## Additional Resources
- [Online playground](https://pbaille.github.io/noon/)
- See `doc/noon.org` for comprehensive documentation
- See `doc/eval.org` for evaluation details
