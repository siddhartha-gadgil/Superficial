![scala CI](https://github.com/siddhartha-gadgil/Superficial/workflows/scala%20CI/badge.svg)
[![Gitter](https://badges.gitter.im/siddhartha-gadgil/Superficial.svg)](https://gitter.im/siddhartha-gadgil/Superficial?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)
## Superficial

Our goal is to implement various representations and algorithms for surfaces and structures on these. The implementation will be in _scala_.

### Quick Start:

Ensure that you have Java 8 installed. In a linux system, you can start a console with the latest version of the code compiled (plus `import superficial` run in advance) by running the following:

```bash
./repl.sh
```

alternatively, you can start a `mill` REPL with 
```bash
./mill -i superficial.repl
```

but make sure to give the repl command `import superficial._`.

## Other stuff

For the sake of convenience, this same repository was also used in the PolyMath 14 project, so there is code related to free groups and lengths on them here.