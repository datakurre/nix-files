{ config, pkgs, ... }:
{
  services.xserver.inputClassSections = [
    ''
      Identifier   "Marble Mouse"
      MatchProduct "Logitech USB Trackball"
      MatchIsPointer "on"
      MatchDevicePath "/dev/input/event*"
      Driver "evdev"
      Option "SendCoreEvents"          "true"
      Option "ButtonMapping"           "1 9 3 4 5 6 7 2"
      Option "EmulateWheel"            "true"
      Option "EmulateWheelButton"      "8"
      Option "YAxisMapping"            "4 5"
      Option "XAxisMapping"            "6 7"
      Option "DeviceAccelProfile"      "3"
      Option "AccelerationProfile"     "2"
      Option "AdaptiveDeceleration"    "2"
      Option "AccelerationNumerator"   "2"
      Option "AccelerationDenominator" "1"
      Option "AccelerationThreshold"   "4"
    ''
  ];
}
