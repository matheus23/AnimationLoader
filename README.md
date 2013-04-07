AnimationLoader
===============

![Example filtered Animation](http://i.imgur.com/gvegndZ.gif)

This project features:
----------------------
* Loading animations from .xml files.
* Filters being defined in the .xml and applied by the program.
* Total generic-ness. This runtime can be used with any backend: Java2D, LWJGL, JogAmp, LibGDX.
A backend for Java2D was implemented in the package org.matheusdev.animationSwing

The animation at the top was generated using this xml:
```xml
<resources>
   <images>
      <image file="sheet.png">
         <!-- Sorcerer -->
         <animation name="sorcerer" delay="0.1">
            <filters>
               <filter name="replace(255, 255, 255, 255 with 1, 1, 1, 0 range 2)" />
               <filter name="colorize_grayscale(0.4, 0.9, 0.3, 1)" />
               <filter name="expand(4, 4)" />
               <filter name="scale(4, 4)" />
               <filter name="voxelize(4, 4 with 0.02, 0.02, 0.02, 1)" />
               <filter name="scale(2, 2)" />
               <filter name="outline(0, 0, 0, 1)" />
               <branch merge="blend(bottom over top)" >
                  <filter name="colorize(0, 0, 0, 1)" />
                  <filter name="blur(8, 8, 16)" />
               </branch>
            </filters>
            <frame bounds="0 0 16 16" />
            <frame bounds="16 0 16 16" />
            <frame bounds="32 0 16 16" />
            <frame bounds="48 0 16 16" />
         </animation>
      </image>
   </images>
</resources>
```

And this sprite-sheet (scaled 8x):
![Example sprite sheet, scaled 8x](http://i.imgur.com/Nsid1E8.png)