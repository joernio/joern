package io.joern.jssrc2cpg.slicing

import io.joern.dataflowengineoss.slicing.*
import io.joern.jssrc2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.Operators

class TsUsageSliceTests extends DataFlowCodeToCpgSuite {

  private val config = UsagesConfig().withParallelism(1)

  "extracting a usage slice from a TypeScript module from the wild" should {

    val cpg = code(
      """import Loader from './loader'; // Used to load files from the web server
        |import Input from './input'; // Used to manage the user input
        |
        |//This is the abstract base of all scenes
        |export abstract class Scene {
        |    game: Game;
        |    gl: WebGL2RenderingContext;
        |    public constructor(game: Game){
        |        this.game = game;
        |        this.gl = game.gl;
        |    }
        |
        |    public abstract load(): void; // Here we will tell the loader which files to load from the webserver
        |    public abstract timer(): void;
        |    public abstract start(): void; // Here we will initialize the scene objects before entering the draw loop
        |    public abstract draw(deltaTime: number): number; // Here will draw the scene (deltaTime is the difference in time between this frame and the past frame in milliseconds)
        |    public abstract end(): void; // Here we free the memory from objects we allocated
        |}
        |
        |//This class create the WebGL2 context, manages the scenes and handles the game loop
        |export default class Game {
        |    canvas: HTMLCanvasElement; // The canvas on which we will draw
        |    gl: WebGL2RenderingContext; // The WebGL2 context of the canvas (we will use it to draw)
        |    loader: Loader = new Loader(); // A loader to read files from the webserver
        |    input: Input; // A manager for user input (keyboard and mouse)
        |    scenes: {[name: string]: Scene} = {}; // A dictionary of all available scenes
        |    currentScene: Scene = null; // The scene that is currently being drawn
        |    nextScene: Scene = null; // The scene that will replace the current scene after its files have been loaded
        |    nextSceneReady: boolean = false; // Whether the files requested by the next scene has been loaded or not
        |    lastTick: number; // The time of the last frame in milliseconds (used to calculate delta time)
        |
        |    constructor(canvas: HTMLCanvasElement){
        |        this.canvas = canvas;
        |        this.gl = this.canvas.getContext("webgl2", {
        |            preserveDrawingBuffer: true, // This will prevent the Browser from automatically clearing the frame buffer every frame
        |            alpha: true, // this will tell the browser that we want an alpha component in our frame buffer
        |            antialias: true, // this will tell the browser that we want antialiasing
        |            depth: true, // this will tell the browser that we want a depth buffer
        |            powerPreference: "high-performance",
        |            premultipliedAlpha: false, // This can be used if the canvas are going to be blended with the rest of the webpage (transparency)
        |            stencil: true // this will tell the browser that we want a stencil buffer
        |        }); // This command loads the WebGL2 context which we will use to draw
        |        this.input = new Input(this.canvas);
        |        this.lastTick = performance.now();
        |        this.loop(performance.now()); // Start the game loop
        |    }
        |
        |    public addScene(name: string, type: new (game: Game) => Scene){
        |        this.scenes[name] = new type(this);
        |    }
        |
        |    public addScenes(scenes: {[name: string]: new (game: Game) => Scene}){
        |        for(let name in scenes) this.addScene(name, scenes[name]);
        |    }
        |
        |    public startScene(name: string){
        |        if(name in this.scenes){
        |            this.nextScene = this.scenes[name];
        |            this.nextSceneReady = false;
        |            this.nextScene.load();
        |            this.loader.wait().then(()=>{this.nextSceneReady = true;}) // This will make the loader notify us when the files are ready
        |        } else {
        |            console.warn(`Scene "${name}" not found`);
        |        }
        |    }
        |
        |    private loop(time: DOMHighResTimeStamp){
        |        requestAnimationFrame((time) => this.loop(time)); // Tell the browser to call this function again when the next frame needs to be drawn
        |        if(this.nextScene != null && this.nextSceneReady){ // If there is a next scene and it is ready, replace the current scene with it.
        |            if(this.currentScene != null) this.currentScene.end(); // If there was an old scene, tell it to free its memory
        |            this.currentScene = this.nextScene;
        |            this.nextScene = null;
        |            this.currentScene.start(); // Tell the scene to initialize its objects
        |        }
        |        // this.currentScene = this.nextScene;
        |        // this.currentScene.start();
        |        if(this.currentScene != null){
        |            let state = this.currentScene.draw(time-this.lastTick); // Tell the scene to draw itself
        |            if (state == -1)
        |            {
        |                console.log("Game");
        |                return;
        |            }
        |        }
        |        this.input.update(); // Update some information about the user input
        |        this.lastTick = time;
        |    }
        |
        |}
        |
        |""".stripMargin,
      "main.ts"
    )
    val programSlice = UsageSlicing.calculateUsageSlice(cpg, config.copy(excludeOperatorCalls = false))

    "extract 'loader' object slice from the main program" in {
      val slice = programSlice.objectSlices.find(x => x.fullName == "main.ts::program").flatMap(_.slices.headOption).get
      slice.definedBy shouldBe Some(CallDef("new Loader", "Loader", Option("Loader"), Some(24), Some(21)))
      slice.targetObj shouldBe LocalDef("loader", "loader:Loader")

      val inv1 = slice.invokedCalls.find(_.callName == "Loader").get

      inv1.returnType shouldBe "Loader"
    }

    "extract 'time' parameter slice from the lambda in 'loop'" in {
      val slice =
        programSlice.objectSlices
          .find(x => x.fullName == "main.ts::program:Game:loop:<lambda>1")
          .flatMap(_.slices.headOption)
          .get
      slice.definedBy shouldBe Some(ParamDef("time", "ANY", 1, Some(68), Some(31)))
      slice.targetObj shouldBe ParamDef("time", "ANY", 1, Some(68), Some(31))

      val arg1 = slice.argToCalls.find(_.callName == "loop").get

      arg1.position shouldBe Right(1)
      arg1.paramTypes shouldBe List("DOMHighResTimeStamp")
      arg1.returnType shouldBe "ANY"
    }

  }

}
