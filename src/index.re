open Reprocessing;

let canvasSize = 2500;

let polandWidth = 100;
let polandHeight = 100;

let colors = [|
  (
    Utils.colorf(~r=1.0, ~g=0.0, ~b=0.0, ~a=0.5),
    Utils.colorf(~r=1.0, ~g=0.0, ~b=0.0, ~a=1.0)
  ),
  (
    Utils.colorf(~r=0.0, ~g=1.0, ~b=0.0, ~a=0.5),
    Utils.colorf(~r=0.0, ~g=1.0, ~b=0.0, ~a=1.0)
  ),
  (
    Utils.colorf(~r=0.0, ~g=0.0, ~b=1.0, ~a=0.5),
    Utils.colorf(~r=0.0, ~g=0.0, ~b=1.0, ~a=1.0)
  )
|];

let randomColor() {
  let nth = Random.int(Array.length(colors));
  colors[nth];
};

type blob = {
  x: int,
  y: int,
  col: (colorT, colorT)
};

type draw_state = {
  images: list(imageT)
};

type play_state = {
  players: list(imageT),

  playerPosition: (int, int),
  playerSize: int,
  playerColorStroke: colorT,
  playerColorFill: colorT,
  playerMovement: (float, float),

  blobs: list(blob),
  score: int
};

type state =
  | Draw(draw_state)
  | Play(play_state);

let setup(env) {
  Env.size(~width=600, ~height=600, env);
  Env.resizeable(true, env);

  Draw({
    images: [Draw.createImage(~width=polandWidth, ~height=polandHeight, env)]
  });
};

let drawDraw(state, env) {
  if (Env.keyPressed(Space, env)) {
    let numBlobs = Utils.random(~min=250, ~max=500);

    let (stroke, fill) = randomColor();
    Play({
      players: state.images,

      playerPosition: (300, 300),
      playerSize: 20,
      playerColorStroke: stroke,
      playerColorFill: fill,
      playerMovement: (0.0, 0.0),

      blobs: Array.init(numBlobs, (_) => {
        x: Utils.random(~min=0, ~max=canvasSize),
        y: Utils.random(~min=0, ~max=canvasSize),
        col: randomColor()
      }) |> Array.to_list,
      score: 0
    });
  } else if (Env.keyPressed(R, env)) {
    Draw({
          images: [Draw.createImage(~width=polandWidth, ~height=polandHeight, env),
                   ...List.tl(state.images)]
    });
  } else {
    let xOff = Env.width(env) / 2;
    let yOff = Env.height(env) / 2;

    let currImage = List.hd(state.images);

    Draw.background(Constants.white, env);

    Draw.image(currImage, ~pos=(xOff, yOff), env);

    /* Draw outline of body */
    Draw.stroke(Constants.black, env);
    Draw.strokeWeight(8, env);
    Draw.noFill(env);
    /* Should cover the entire area... does this include strokeWeight? */
    Draw.ellipse(~center=(xOff + polandWidth / 2, yOff + polandHeight / 2), ~radx=polandWidth / 2, ~rady=polandHeight / 2, env);

    /* Eyes */
    Draw.stroke(Constants.black, env);
    Draw.strokeWeight(3, env);
    Draw.fill(Constants.white, env);
    Draw.ellipse(~center=(xOff + polandWidth / 3, yOff + polandHeight / 3), ~radx=8, ~rady=8, env);
    Draw.ellipse(~center=(xOff + 2 * polandWidth / 3, yOff + polandHeight / 3), ~radx=8, ~rady=8, env);

    /* Draw UI */
    Draw.text(~body="Use the mouse to draw..",  ~pos=(0, 0), env);
    Draw.text(~body="Press 'r' to clear.",      ~pos=(0, 20), env);
    Draw.text(~body="Press space to continue.", ~pos=(0, 40), env);

    if (Env.mousePressed(env)) {
      let drawPixel(env) {
        let (mx, my) = Env.mouse(env);
        Draw.noStroke(env);
        Draw.fill(Constants.black, env);
        Draw.ellipse(~center=(mx - xOff, my - yOff), ~radx=3, ~rady=3, env);
      };

      Draw.withImage(currImage, env, drawPixel);
    };
    Draw(state);
  };
};

let drawPlay(state, env) {
  let halfWidth = Env.width(env) / 2;
  let halfHeight = Env.height(env) / 2;

  Draw.background(Constants.white, env);

  /* Draw blobs */
  let drawBlob({x, y, col: (stroke, fill)}) {
    let pos = switch state.playerPosition {
      /* Shift by a halfWidth/halfHeight to match the transform to playerPosition */
      | (cx, cy) => (x - cx + halfWidth, y - cy + halfHeight)
    };

    Draw.stroke(stroke, env);
    Draw.strokeWeight(5, env);
    Draw.fill(fill, env);
    Draw.ellipse(~center=pos, ~radx=10, ~rady=10, env);
  };

  List.iter(drawBlob, state.blobs);

  /* Draw player */
  Draw.stroke(state.playerColorStroke, env);
  Draw.strokeWeight(15, env);
  Draw.fill(state.playerColorFill, env);
  Draw.ellipse(~center=(halfWidth, halfHeight), ~radx=state.playerSize, ~rady=state.playerSize, env);

  /* Draw UI */
  Draw.text(~body="Your score: " ++ string_of_int(state.score), ~pos=(0, 0), env);

  /* Calculate new center/player position after player movement */
  let center = switch (state.playerPosition, state.playerMovement) {
    | ((x, y), (dx, dy)) => {
      let speed = 100.0 /. log(float(state.playerSize));
      let int_dx = int_of_float(dx *. speed);
      let int_dy = int_of_float(dy *. speed);
      /* If the player is at 0, we can think of that as camera focused at 0 */
      Utils.(
        constrain(~amt=(x + int_dx), ~low=0, ~high=canvasSize),
        constrain(~amt=(y + int_dy), ~low=0, ~high=canvasSize)
      );
    }
  };

  /* Calculate new movement vector */
  let (mouseX, mouseY) = Env.mouse(env);
  let (xDiff, yDiff) = Utils.(
    constrain(~amt=(mouseX - halfWidth), ~low=-halfWidth, ~high=halfWidth),
    constrain(~amt=(mouseY - halfHeight), ~low=-halfHeight, ~high=halfHeight)
  );

  let xClamp = Utils.norm(~value=float(xDiff), ~low=0.0, ~high=float(halfWidth));
  let yClamp = Utils.norm(~value=float(yDiff), ~low=0.0, ~high=float(halfHeight));

  /* Test collision with player and blobs */
  let testCollisions() {
    let noCollision({x, y}) {
      let psize = state.playerSize;
      let (pMinX, pMaxX, pMinY, pMaxY) = switch state.playerPosition {
        | (px, py) => (px - psize, px + psize, py - psize, py + psize)
      };

      (x < pMinX || x > pMaxX) || (y < pMinY || y > pMaxY);
    };

    let survivors = List.filter(noCollision, state.blobs);
    (survivors, List.length(state.blobs) - List.length(survivors))
  };

  let (survivors, dscore) = testCollisions();

  Play({
    ...state,
    playerPosition: center,
    playerSize: state.playerSize + dscore,
    playerMovement: (xClamp, yClamp),

    blobs: survivors,
    score: state.score + dscore
  });
};

let draw(state, env) {
  switch state {
    | Draw(draw) => drawDraw(draw, env)
    | Play(play) => drawPlay(play, env)
  };
};

let () =
  run(~setup, ~draw, ());
