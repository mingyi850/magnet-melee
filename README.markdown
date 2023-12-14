# Magnet Melee

![thumbnail](public/thumbnail.png)

Magnet melee is a physics-based strategy game where you compete with other players on the board to gain influence. 
Players take turns to place magnets of either positive or negative polarities which exerts a force on other pieces on the board.
The winner of the game is the player with the highest magnetic force exerted on the board.

This game was developed independently as part of Professor Dennis Shasha's Heuristic Problem Solving course in NYU's MSCS program.

Special thanks to @wjmn for providing the [game template](https://github.com/wjmn/drecco-game-template) which this game is built on.

# Developing

Make sure you have [Elm](https://elm-lang.org/) and [`create-elm-app`](https://github.com/halfzebra/create-elm-app) installed. 

To run the development server inside the repository:

```
elm-app start
```

To build the application, run the build script from inside the repository (make sure you have execution permissions):

```
./build.sh
```

# Directory Structure

Within the root directory of this project:
1. `elm.json` contains information used by Elm (e.g. dependencies). Don't modify this file directly. 
2. `build.sh` is the script used to build a production build ready for deployment to the Dr Ecco website. This replaces the build folder with a new build.  
3. The `src` directory contains all the actual game code. Main game code is located in Main.elm, Settings.elm and Game.elm whereas the 
4. The `public` directory contains assets that are copied directly to the final production build. 
5. The `tests` directory contains tests for the Elm code.
6. The `elm-stuff` directory (ignored by Git) contains Elm dependencies. Don't modify this folder directly. 
7. The `build` directory (ignored by Git) contains the production build produced by `build.sh`. **Don't try to make changes to files in this folder** as they will just be deleted when `build.sh` is rerun. Leave any changes to files in this folder up to `build.sh`.

Within the `src` folder:
1. `src/Main.elm` is the entrypoint that handles delegating to the Settings and Game screen views. 
2. `src/Common.elm` contains basic utility types and functions shared by both `Settings.elm` and `Game.elm`.
3. `src/Settings.elm` contains the model, view and update for the Settings screen. 
4. `src/Game.elm` contains the model, view and update for the Gameplay screen
5. `src/Models.elm` Contains the implementation for the game board
6. `src/BoardCellGrid.elm` Is taken and modified from elm package `jxxcarlson/elm-cell-grid`. It contains rendering functions for the board implementation.
7. `src/Utils` folder contains all utility functions for physics, color and vector manipulation
8. `src/index.js` contains the JavaScript entrypoint for the live development server. This file is not used for the production build. If you modify this file, you **also** need to make sure that you make the same changes to `public/index.html` so that these changes will be reflected in the production build. 
9. `src/main.css` contains all the CSS styling. I recommend just keeping all your CSS styles in this one file and modify it as necessary. **If you add another CSS file, you need to add references to it in both `src/index.js` (for the live development server) and `public/index.html` (for the production build).**

Within the `public` folder, there are three essential static assets:
1. `public/index.html` contains the HTML entrypoint that is used by both the live development server and the production build. 
2. `public/index.md` is a Markdown file that gets converted to the information page for the game.
3. `public/thumbnail.png` is a square image used as the thumbnail for the game.