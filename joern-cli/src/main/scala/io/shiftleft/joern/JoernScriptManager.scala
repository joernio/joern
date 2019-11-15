package io.shiftleft.joern

import io.shiftleft.console.ScriptManager

class JoernScriptManager(executor: JoernScriptExecutor = new JoernScriptExecutor()) extends ScriptManager(executor)