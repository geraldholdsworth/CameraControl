# CameraControl
This is a simple application to control a Tandberg/Cisco PrecisionHD video conferencing camera.<br>
As these cameras abide by the VISCA standard, this should also control any other camera following the same standard.<br>
Note the RS232 pinout in the Tandberg, and later Cisco, manuals is wrong. Instead of pin 3 from the RJ45 going to pin 2 on the D-SUB, and pin 6 on the RJ45 going to pin 3 on the D-SUB, it should be the other way round:<br>
RJ45 pin 3 (RX) -> D-SUB pin 3 (TX)<br>
RJ45 pin 6 (TX) -> D-SUB pin 2 (RX)<br>
<br>
Project was written in Lazarus (https://www.lazarus-ide.org). Note that although I have supplied a binary for macOS, it is untested and is likely not to work, as I had to bodge the serial.pp file to get it to compile (as my main development machine is a Mac, but this camera is connected to my Windows PC). Full source is available if you wish to compile for other systems.<br>
<br>
If you want, you can support this project, and others, by buying me a coffee (or a tea/beer/rum/etc.): https://ko-fi.com/geraldholdsworth<br>
