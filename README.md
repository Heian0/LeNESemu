This is a NES Emulator written in C++ for playing NES games inside Unreal Engine. It currently only supports Mapper 0, so not many games are playable. The PPU outputs an array of FColor objects, which are then applied to a Dynamic Material Instance in Unreal. In the video attached, the emulator is running Super Mario Bros (Ignore the fact that the desk has an Atari on it).


https://github.com/user-attachments/assets/cd8b069f-ea2b-406e-8630-3f19459b2368


There are some issues with the rendered image (the screen object is a bit too big for what the NES actually outputs which is 256x240). Below is a video of the game fully rendering to a basic Plane object in Unreal.


https://github.com/user-attachments/assets/6934e51f-5ea0-43fa-8ac4-83f869c913b9


Down the line it may be interesting to try to have the PPU render to actual objects in the scene and not just the screen to potentially create an HD2D style NES emulator for top down games such as the original Legend of Zelda, or have it to render a screen or physical objects in an AR scene. Below is another video showcasing a scene made in Unity of the HD2D style of games.


https://github.com/user-attachments/assets/6d02e7bd-ed7e-4f23-962f-8c65a93d671e

