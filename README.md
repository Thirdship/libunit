# libunit
Libunit is released under the [Apache v 2.0](License.txt).

Travis: [![Build Status](https://travis-ci.org/Thirdship/libunit.svg?branch=master)](https://travis-ci.org/Thirdship/libunit)

Circle: [![Circle CI](https://circleci.com/gh/Thirdship/libunit/tree/master.svg?style=svg)](https://circleci.com/gh/Thirdship/libunit/tree/master)

## What it does
libunit is a general purpose united number system implemented in Scala. The purpose is to allow storage, searching, and conversion of measures between units. Units are both implemented raw, such as with length and time as ConvertibleTSUnits, as well as constructed as compound units in ComputableTSUnit. AStarSolver allows conversion between any kind of ConvertibleTSUnit, by treating units as nodes, conversions as connections, and finding the least-lossy conversion via an A* Algorithm.

```Scala
10.Meters // is represented in the system as a Length and as Meters.
```
