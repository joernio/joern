package io.joern.pythonparser;

public class PositionToken {
    public int startPos = -1;
    // Non inclusive. Points behind the last token character.
    public int endPos = -1;
}
