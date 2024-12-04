This is a NES Emulator written in C++ for playing NES games inside Unreal Engine. It currently only supports Mapper 0, so not many games are playable. The PPU outputs an array of FColor objects, which are then applied to a Dynamic Material Instance in Unreal. In the video attached, the emulator is running Super Mario Bros.


https://github.com/user-attachments/assets/92ce693d-8a52-47ab-a486-7ec4cee870a8


There are some issues with the rendered image (the screen object is a bit too big for what the NES actually outputs which is 256x240). Below is a video of the game fully rendering to a basic Plane object in Unreal.


https://github.com/user-attachments/assets/6a035f2d-b6ba-4ba3-b047-3cdfd628750d


Down the line it may be interesting to try to have the PPU render to actual objects in the scene and not just the screen to potentially create an HD2D style NES emulator for top down games such as the original Legend of Zelda, or have it to render a screen or physical objects in an AR scene. Below is another video showcasing a scene made in Unity of the HD2D style of games.


https://github.com/user-attachments/assets/324f8501-50b8-4e4c-a88f-8fcf77aac3d0
