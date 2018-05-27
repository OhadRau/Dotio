open Reprocessing;

let canvasSize = 2500;

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

type state = {
  playerPosition: (int, int),
  playerSize: int,
  playerColorStroke: colorT,
  playerColorFill: colorT,
  playerMovement: (float, float),

  blobs: list(blob),
  score: int
};

let setup(env) {
  Env.size(~width=600, ~height=600, env);
  Env.resizeable(true, env);

  let numBlobs = Utils.random(~min=250, ~max=500);

  let (stroke, fill) = randomColor();
  {
    playerPosition: (300, 300),
    playerSize: 20,
    playerColorStroke: stroke,
    playerColorFill: fill,
    playerMovement: (0.0, 0.0),

    blobs: Array.init(numBlobs, i => {
      x: Utils.random(~min=0, ~max=canvasSize),
      y: Utils.random(~min=0, ~max=canvasSize),
      col: randomColor()
    }) |> Array.to_list,
    score: 0
  };
};

let draw(state, env) {
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

  {
    ...state,
    playerPosition: center,
    playerSize: state.playerSize + dscore,
    playerMovement: (xClamp, yClamp),

    blobs: survivors,
    score: state.score + dscore
  };
};

let () =
  run(~setup, ~draw, ());
