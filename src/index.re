open Reprocessing;

[@bs.val] external windowInnerWidth : int = "window.innerWidth";
[@bs.val] external windowInnerHeight : int = "window.innerHeight";

let drawWidth = 400;
let drawHeight = 400;

let colorPickerWidth = 60;
let colorPickerHeight = 400;

let colorWidth = 40;
let colorHeight = 40;

let themes = ["dog", "car", "ball", "airplane", "hammer", "run", "eat"];

let randomTheme() {
  List.nth(themes, Utils.random(~min=0, ~max=List.length(themes) - 1));
};

type draw_state = {
  theme:   string,
  color:   colorT,
  image:   imageT,
  lastPos: option((int, int))
};

type state =
  | Draw(draw_state);

let setup(env) {
  Env.size(~width=windowInnerWidth, ~height=windowInnerHeight, env);
  Env.resizeable(true, env);

  Draw({
    theme: randomTheme(),
    color: Constants.black,
    image: Draw.createImage(~width=drawWidth, ~height=drawHeight, env),
    lastPos: None
  });
};

let drawDraw(state, env) {
  /* Half of the screen, minus the drawing area */
  let xOff = (Env.width(env) - drawWidth) / 2;
  let yOff = (Env.height(env) - drawHeight) / 2;

  /* Clear the screen */
  Draw.background(Constants.white, env);

  /* Draw the drawing area as it currently exists */
  Draw.image(state.image, ~pos=(xOff, yOff), env);

  /* Draw the UI: */

  /* Draw an outline around the drawing area */
  Draw.stroke(Constants.black, env);
  Draw.strokeWeight(2, env);
  Draw.noFill(env);
  /* Make room for a 2px black border around the area */
  Draw.rect(~pos=(xOff - 2, yOff - 2), ~width=drawWidth + 4, ~height=drawHeight + 4, env);

  /* Draw an outline around the color picker */
  let cpXOff  = xOff + drawWidth + (xOff - colorPickerWidth) / 2;
  Draw.rect(~pos=(cpXOff - 2, yOff - 2), ~width=colorPickerWidth + 4, ~height=colorPickerHeight + 4, env);

  /* Draw the colors */
  let cXOff = cpXOff + (colorPickerWidth - colorWidth) / 2;
  let cYGap = 10;

  Draw.stroke(Constants.black, env);
  Draw.strokeWeight(2, env);

  let drawColor(i, col) {
    Draw.fill(col, env);
    Draw.rect(~pos=(cXOff, yOff + i * colorHeight + (i + 1) * cYGap), ~width=colorWidth, ~height=colorHeight, env);
  };

  let cols = [Constants.black, Constants.white, Constants.red, Constants.blue, Constants.green];
  List.iteri(drawColor, cols);

  /* Draw instructions */
  Draw.text(~body="Your theme: " ++ state.theme,   ~pos=(0, 0), env);
  Draw.text(~body="Press 'r' to clear.",           ~pos=(0, 30), env);
  Draw.text(~body="Press space to continue.",      ~pos=(0, 60), env);

  if (Env.mousePressed(env)) {
    let (mx, my) = Env.mouse(env);
    if (mx >= cXOff && mx <= cXOff + colorWidth &&
        (my - yOff - cYGap) mod (colorHeight + cYGap) <= colorHeight) {
      let colorNum = (my - yOff - cYGap) / (colorHeight + cYGap);
      let col = List.nth(cols, colorNum);
      Draw({
        ...state,
        color: col,
        lastPos: None
      });
    } else if (mx >= xOff && mx <= xOff + drawWidth &&
               my >= yOff && my <= yOff + drawHeight) {
      let drawPixel(env) {
        switch state.lastPos {
          | None => {
              Draw.noStroke(env);
              Draw.fill(state.color, env);
              Draw.ellipse(~center=(mx - xOff, my - yOff), ~radx=3, ~rady=3, env)
            }
          | Some(p1) => {
              Draw.stroke(state.color, env);
              Draw.strokeWeight(6, env); /* 2 x radius */
              Draw.noFill(env);
              Draw.line(~p1, ~p2=(mx - xOff, my - yOff), env);
            }
        };
      };

      Draw.withImage(state.image, env, drawPixel);

      Draw({
        ...state,
        lastPos: Some((mx - xOff, my - yOff))
      });
    } else {
      Draw({
        ...state,
        lastPos: None
      });
    };
  } else if (Env.keyPressed(R, env)) {
    Draw({
      ...state,
      color: Constants.black,
      image: Draw.createImage(~width=drawWidth, ~height=drawHeight, env),
      lastPos: None
    });
  } else {
    Draw({
      ...state,
      lastPos: None
    });
  };
};

let draw(state, env) {
  switch state {
    | Draw(draw) => drawDraw(draw, env)
  };
};

let () =
  run(~setup, ~draw, ());
