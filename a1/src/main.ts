import { interval, fromEvent, zip, merge, concat, Subscription } from "rxjs";
import { map, filter, scan, take, refCount } from "rxjs/operators";
import "./style.css";

function main() {

  /**
   * This is the view for your game to add and update your game elements.
   */
  const svg = document.querySelector("#svgCanvas") as SVGElement & HTMLElement;

  type GroundType = 'frog' |'car' | 'log'  | 'water' | 'target' | 'completedTarget' | 'racecar'

  // Two types of game state transitions
  class Tick { constructor(public readonly elapsed:number) {} }
  class Translation { constructor(public readonly axis:string, readonly magnitude:number) {} }

  

  const 
    // Constants that will not change throughout the game, maybe reffered back to when
      // restarting the game.
    Constants = {
      StartScore: 0,
      CanvasSize: 600,
      LevelSVG: {
        id: 'level',
        x: 523,
        y: 570,
        fill: 'white'
      },
      FinalMessageSVG: {
        id: 'gameover',
        x: 150,
        y: 300,
        fill: 'white',
      },
      ScoreSVG: {
        id: 'score',
        x: 520,
        y: 590,
        fill: 'white'
      },
      FrogStartingPoint: {x: 285, y: 560},
      FrogDimensions: {
        width: 30,
        height: 30,
        fill: "greenYellow"
      },
      CarConstants: {
        width: 50,
        height: 40,
        fill: "red"
      },
      RaceCarConstants: {
        width: 50,
        height: 40,
        fill: "#00FFFF"
      },
      LogConstants: {
        width: 150,
        height: 40,
        fill: "#C19A6B	"
      },
      WaterConstants: {
        width: 600,
        height: 300,
        fill: "blue"
      },
      UncompletedTargetConstants: {
        width: 100,
        height: 100,
        fill: "yellow"
      },
      TargetPositions: [{x:0, y:0}, {x: 250, y: 0}, {x:500, y: 0}],
      StartCarPositions: [[{x:196, y:355}, 1], [{x:246, y:355}, 1], [{x:420, y:355}, 1],
                          [{x:230, y:455},-1.5], [{x:0, y:455},-1.5], [{x:420, y:455},-1.5],
                          [{x:196, y:505},-1], [{x:246, y:505},-1], [{x:80, y:505},-1]],

      StartRaceCarPositions: [[{x:100, y:405}, 3], [{x:160, y:405}, 3], [{x:220, y:405}, 3]],
                          
      StartLogPositions: [[{x:150, y:105}, 1], [{x:380, y:105}, 1],[{x:540, y:105}, 1],
                          [{x:200, y: 155},-1], [{x:500, y: 155},-1], [{x:0, y: 155},-1],
                          [{x:22, y:205}, 1], [{x:300, y:205}, 1], 
                          [{x:300, y: 255},-1], [{x:50, y: 255},-1]]

      
    } as const

  // Type for a grounds position
  type Position = Readonly<{
    x: number
    y: number
  }>
  // Dimension of ground types. Important in the collision mechanics between ground items
  type Dimension = Readonly<{
    width: number,
    height: number,
    fill: string
  }>
  // Ground type, these are the main building block of the game
  type Ground = Readonly<{
    id:string,
    dim: Dimension,
    pos:Position,
    speed:number,
    viewType: GroundType,
  }>
  // Curried function responsible for creating instances of ground.
  const createGround = (viewType: GroundType)=> (id: number)=> (initPos:Position)=> (speed: number) => (dim: Dimension) =>
   <Ground>{
    id: viewType + id,
    pos: initPos,
    speed: speed,
    viewType: viewType,
    dim: dim
  }

  // The game state in which the entire game runs off, new instances of this
    // will be created throughout the game running, with modifications that occurring
    // as controlled by the observable stream
  type State = Readonly<{
    time:number,
    frog:Ground,
    cars:ReadonlyArray<Ground>,
    racecars:ReadonlyArray<Ground>,
    logs: ReadonlyArray<Ground>,
    uncompletedTarget: ReadonlyArray<Ground>,
    completedTarget: ReadonlyArray<Ground>,
    remove: ReadonlyArray<Ground>,
    water: Ground,
    score: number,
    gameWin: boolean
    gameOver:boolean
    level: number
  }>

  const 
    // higher order functions mapping multiple instances of ground determined in our constants
    initializeCars = Constants.StartCarPositions
      .map((attr, index)=> createGround("car")(index)(attr[0])(attr[1])(Constants.CarConstants)),

    initializeRaceCars = Constants.StartRaceCarPositions
      .map((attr, index)=> createGround("racecar")(index)(attr[0])(attr[1])(Constants.RaceCarConstants)),

    initalizeLogs = Constants.StartLogPositions
      .map((attr, index)=> createGround("log")(index)(attr[0])(attr[1])(Constants.LogConstants)),

    initalizeTargets = Constants.TargetPositions
      .map((pos, index)=> createGround("target")(index)(pos)(0)(Constants.UncompletedTargetConstants))


  const initialState: State = {
    time:0,
    frog: createGround('frog')(0)(Constants.FrogStartingPoint)(0)(Constants.FrogDimensions),
    cars: initializeCars,
    racecars: initializeRaceCars,
    logs: initalizeLogs,
    water: createGround('water')(0)({x: 0, y: 0})(0)(Constants.WaterConstants),
    uncompletedTarget: initalizeTargets,
    completedTarget: [],
    remove: [],
    gameWin: false,
    gameOver: false,
    score: Constants.StartScore,
    level: 1
  }
  // Imperative function that has external change outside the function. Mainly responsible for
    // changing the SVG canvas
  function updateView(s: State): void {
    function nonDynamic(): void {
      const
      // Responsible for creating the non dynamic SVG items
      createNonDynamicSVG = ():void => {
        const
          level = document.createElementNS(svg.namespaceURI, 'text'),
          score = document.createElementNS(svg.namespaceURI, 'text'),
          levelAttr = Object.entries(Constants.LevelSVG),
          scoreAttr = Object.entries(Constants.ScoreSVG)
          levelAttr.forEach(([key, val]) => level.setAttribute(String(key), String(val)))
          level.textContent = 'Level:' + String(s.level)
          scoreAttr.forEach(([key, val]) => score.setAttribute(String(key), String(val)))
          score.textContent = 'Score:' + String(s.score)
          svg.appendChild(level)
          svg.appendChild(score)
      },
      // Updating the non dynamic SVG 
      updateNonDynamicSVG = ():void => {
        const
          level = document.getElementById(Constants.LevelSVG.id),
          score = document.getElementById(Constants.ScoreSVG.id)
          level?.textContent = 'Level:' + String(s.level)
          score?.textContent = 'Score:' + String(s.score)
      }
      s.time === 0? createNonDynamicSVG(): updateNonDynamicSVG()
    }
    // function that controls that SVGs of the dynamically moving ground
    function dynamicEntities(b: Ground): void {
      const
      pos = Object.entries(b.pos),
      dim = Object.entries(b.dim),
        // Updating of these SVG
        updateSVG = ():void => {
          const
            element = document.getElementById(b.id)
            pos.forEach(([key, val]) => element.setAttribute(String(key), String(val)))
        },
        // Creation of SVG
        createSVG = (): void => {
          const
            element = document.createElementNS(svg.namespaceURI, "rect")!
              element.setAttribute("id", b.id)
              merge(dim, pos)
                .forEach(([key, val]) => element.setAttribute(String(key), String(val)))             
              svg.appendChild(element)
        }
      document.getElementById(b.id) ? updateSVG(): createSVG()
    }
    const
      // helper function for removing any particular SVG
      removeSVG = (b: Ground): void => {
        document.getElementById(b.id)?.remove()
      },
    
    // higher order functions that use our previously declared functions to map over all ground units.
    dynamicBodies = concat(s.cars, s.racecars, [s.water], s.logs, s.uncompletedTarget, s.completedTarget, [s.frog])
    dynamicBodies.subscribe(dynamicEntities)
    s.remove.map(removeSVG)
    nonDynamic()

    // Conditional for a game loss via a collision. Unsubscribes from stream
    if (s.gameOver) {
      gameStream.unsubscribe()
      const 
        finalMessage = document.createElementNS(svg.namespaceURI, 'text'),
        msgAttr = Object.entries(Constants.FinalMessageSVG)
        msgAttr.forEach(([key, val]) => finalMessage.setAttribute(String(key), String(val)))
        finalMessage.setAttribute('font-size', '60px')
        finalMessage.textContent = 'Game Over'
        svg.appendChild(finalMessage)
    }
    // Conditional following a game win. Resets the SVG
    if (s.gameWin) {
      s.cars.forEach(car => document.getElementById(car.id)?.remove())
      s.logs.forEach(log => document.getElementById(log.id)?.remove())
      document.getElementById('frog0')?.remove()
    }    
  }

  const
    // Conditional check for a game win, if true returns initial state with same score and 
      // an increased difficulty
    resetGame = (s: State): State => {
      return s.gameWin ? 
        <State>{...initialState,
          cars: initializeCars.map(r=>updateSpeed(r, s.level+1)),
          racecars: initializeRaceCars.map(r=>updateSpeed(r, s.level+1)),
          logs: initalizeLogs.map(r=>updateSpeed(r, s.level+1)),
          score: s.score,
          level: s.level + 1}: s},
    
    // returns a ground unit with an updated speed.
    updateSpeed = (g: Ground, s: number): Ground => <Ground>{...g,
      speed: g.speed*s
    }

  
  const 
    // handles any incoming observables and dictates what the game does depending on what comes
      // from the stream
    observableHandler = (s: State, e: Tick | Translation): State =>
    e instanceof Tick ? tick(s, e.elapsed): {...s,
      frog: {...s.frog, pos: determinePosition(e, s.frog.pos)}
    },

    // Determines a position based on a given translation.
    determinePosition = (t: Translation, p: Position): Position => {
      const
        // Takes a position and returns a new position depending on whether it is inside
          // the SVG canvas.
        outsideCanvas = (q: Position): Position => 
        (q.x < Constants.CanvasSize && q.x > 0)
          &&
        (q.y < Constants.CanvasSize && q.y > 0) ? q: p
      return t.axis === 'x' ? outsideCanvas({...p, x: p.x + t.magnitude}): outsideCanvas({...p, y: p.y + t.magnitude})
    },

    // Mainly the handling for the ground objects and calls any required function on these units. 
      // returning a state with these modifications applied.
    tick = (s: State, e: number): State => {
    return <State> interactions(resetGame({...s,
      frog: automaticMovement(s.frog),
      cars: s.cars.map(automaticMovement),
      logs: s.logs.map(automaticMovement), 
      racecars: s.racecars.map(automaticMovement),
      time: e,
    }))
  },

    // Functions responsible for interactions between ground objects
    interactions = (s: State): State => {
      const
        // Check for a collisions between a frog and ground object 
        frogCollision = ([frog, Ground]:[Ground,Ground]):boolean => {
          const 
            frogLeft = frog.pos.x,
            frogRight = frog.pos.x + frog.dim.width,
            frogTop = frog.pos.y,
            frogBottom = frog.pos.y + frog.dim.height,
            GroundLeft = Ground.pos.x,
            GroundRight = Ground.pos.x + Ground.dim.width,
            GroundTop = Ground.pos.y,
            GroundBottom = Ground.pos.y + Ground.dim.height
          
        // Checks each case of the frog entering another rectangular bodies dimension.
        return  equalityChecker(GroundLeft, frogLeft, GroundRight) && equalityChecker(GroundTop, frogTop, GroundBottom) ? true:
                equalityChecker(GroundLeft, frogLeft, GroundRight) && equalityChecker(GroundTop, frogBottom, GroundTop) ? true:
                equalityChecker(GroundLeft, frogRight, GroundRight) && equalityChecker(GroundTop, frogTop, GroundBottom) ? true:
                equalityChecker(GroundLeft, frogRight, GroundRight) && equalityChecker(GroundTop, frogBottom, GroundBottom) ? true:
                false
        },
        // Checks whether a frog is within the entire other ground Object
        frogOnGround = ([frog, Ground]:[Ground,Ground]):boolean => {
          const 
          frogLeft = frog.pos.x,
          frogRight = frog.pos.x + frog.dim.width,
          frogTop = frog.pos.y,
          frogBottom = frog.pos.y + frog.dim.height,
          GroundLeft = Ground.pos.x,
          GroundRight = Ground.pos.x + Ground.dim.width,
          GroundTop = Ground.pos.y,
          GroundBottom = Ground.pos.y + Ground.dim.height
            return GroundLeft <= frogLeft && GroundRight >= frogRight && frogBottom <= GroundBottom && frogTop >= GroundTop ? true: false
        },
      // Removes a completed target by checking and filtering all elements with those of the other.
      removeCompletedTarget = (target: ReadonlyArray<Ground>, GroundArray: ReadonlyArray<Ground>): ReadonlyArray<Ground> =>
        target.length > 0 ? GroundArray.filter(r=>r !== target[0]): GroundArray, 
      
      // Checks if the frog is on a log or an uncompleted target using higher order functions
      frogOnLogOrTargets = (s.logs.concat(s.uncompletedTarget).filter(r=>frogOnGround([s.frog, r]))),

      // Checks for collisions that occur between cars or the water using higher order functions
      carCollision = (s.cars).concat(s.racecars).filter(r=>frogCollision([s.frog,r])).length > 0,
      waterCollision = frogCollision([s.frog, s.water]) && frogOnLogOrTargets.length === 0,

      // Updates the speed of the frog if on log or takes the frog back to starting point if it hits a target.
      updateFrogGround = (b: Ground): Ground => {
      return {...b,
        speed: frogOnLogOrTargets.reduce((p, c) => c.speed, 0),
        pos: currentCompletedTarget.length > 0 ? Constants.FrogStartingPoint: b.pos}
      }, 

      // Updates the score if a target is hit
      updateScore = (s: number): number => 
        currentCompletedTarget.length > 0 ? s + 100: s,

      // Creates a new instance of a completed target ground type if the frog hits it.
      createCompletedTargets = s.uncompletedTarget.filter(r=>frogOnGround([s.frog, r]))
        .map((Ground) => 
          createGround("completedTarget")
                    (s.completedTarget.length)
                    (Ground.pos)(0)
                    ({...Constants.UncompletedTargetConstants, fill: 'lightblue'})),

      // returns the target the frog currently completed
      currentCompletedTarget = s.uncompletedTarget.filter(r=>frogOnGround([s.frog, r]))

      // Main return, returning the final state of this tick iterations based on all interactions
        // that have occured during this tick.
      return {
        ...s,
        frog: updateFrogGround(s.frog),
        score: updateScore(s.score),
        uncompletedTarget: removeCompletedTarget(currentCompletedTarget, s.uncompletedTarget),
        completedTarget: s.completedTarget.concat(createCompletedTargets),
        remove: currentCompletedTarget,
        gameOver: carCollision || waterCollision ? true: false,
        gameWin: s.completedTarget.length === 3
      }
    },

    // Helper function to check a double sided equality
    equalityChecker = (a: Number, b: Number, c: Number): boolean => {
      return a <= b && b <= c ? true: false
    },
    
    // Movement for ground objects
    automaticMovement = (b: Ground): Ground => <Ground> {...b,
      pos: horizontalWrapping({...b.pos, x: b.pos.x + (b.speed)}, b)
    },


    // Interval subscription that is mapped to instances of Tick
    clock = 
      interval(10)
      .pipe(map(time => new Tick(time))),

    // Wrapping for ground objects, when leaving the map so they return on the other side.
    horizontalWrapping = (pos: Position, b: Ground): Position => 
      pos.x >= Constants.CanvasSize && b.speed > 0 ? {...pos, x: 0 - b.dim.width}: 
        (pos.x + b.dim.width) <= 0 && b.speed < 0 ? {...pos, x: Constants.CanvasSize} :{...pos}
      
  type Key = 'ArrowLeft' | 'ArrowRight' | 'ArrowUp' | 'ArrowDown' 
  
  // Keybindings function that can map whatever keys to any function depending on the input
  const KeyBinding = <T>(k:Key, f:()=>T)=>
    fromEvent<KeyboardEvent>(document, "keydown")
        .pipe(
          filter(({code})=>code === k),
          map(f)
        )
  
  const
    // Keybindings for all the required keys, that map to the corresponding translation
    leftTranslation = KeyBinding('ArrowLeft',()=>new Translation('x', -50)),
    rightTranslation = KeyBinding('ArrowRight',()=>new Translation('x', 50)),
    upTranslation = KeyBinding('ArrowUp',()=>new Translation('y', -50)),
    downTranslation = KeyBinding('ArrowDown',()=>new Translation('y', 50)),
    
    // the mainn game streams that merges all others, which then scans an accumulation of states and
      // uses that state to update the view.
    gameStream = 
      merge(leftTranslation, rightTranslation, upTranslation, downTranslation, clock)
      .pipe(scan(observableHandler, initialState))
      .subscribe(updateView)
    }

// The following simply runs your main function on window load.  Make sure to leave it in place.
if (typeof window !== "undefined") {
  window.onload = () => {
    main();
  };
}
