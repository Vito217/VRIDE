# Interaction Subsystems

The purpose of this `com.unity.xr.interactionsubsystems` package is to provide definitions of all subsystems that enable XR interaction functionality.

Presently, this package defines the following XR subsystems:
    GestureSubsystem

## Installing Interaction Subsystems

This package is normally not installed by a user, but rather as a dependency defined in other packages.

## Package structure

```none
<root>
  ├── package.json
  ├── README.md
  ├── CHANGELOG.md
  ├── LICENSE.md
  ├── QAReport.md
  └── Runtime
      ├── Unity.XR.InteractionSubsystems.asmdef
      └── GestureSubsystem
```

## Package usage

This package is used as a dependency for other packages in two scenarios:

1. The other package wants to extend the API and implement a provider that provides data to the Subsystems defined in this package.  Packages in this category include:
[Magic Leap XR Plugin](https://docs.unity3d.com/Packages/com.unity.xr.magicleap@1.0/)

1. The other package wants to use the APIs defined in this one.  No current packages in this category.


## Documentation

* [Script API](Runtime/) <update?>
* [Manual](Documentation~/com.unity.xr.interactionsubsystems.md)
