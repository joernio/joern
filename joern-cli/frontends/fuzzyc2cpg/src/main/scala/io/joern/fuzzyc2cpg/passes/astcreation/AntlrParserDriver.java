package io.joern.fuzzyc2cpg.passes.astcreation;

import static org.antlr.v4.runtime.Token.EOF;

import io.shiftleft.codepropertygraph.generated.nodes.NewComment;
import io.joern.fuzzyc2cpg.ast.AstNode;
import io.joern.fuzzyc2cpg.ast.AstNodeBuilder;
import io.joern.fuzzyc2cpg.ast.logical.statements.CompoundStatement;
import io.joern.fuzzyc2cpg.parser.AntlrParserDriverObserver;
import io.joern.fuzzyc2cpg.parser.CommonParserContext;
import io.joern.fuzzyc2cpg.parser.ParserException;
import io.joern.fuzzyc2cpg.parser.TokenSubStream;
import overflowdb.BatchedUpdate.DiffGraphBuilder;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import java.util.function.Consumer;
import org.antlr.v4.runtime.BailErrorStrategy;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.DefaultErrorStrategy;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenSource;
import org.antlr.v4.runtime.misc.ParseCancellationException;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeListener;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import scala.Some;

abstract public class AntlrParserDriver {
    // TODO: This class does two things:
    // * It is a driver for the ANTLRParser, i.e., the parser
    // that creates ParseTrees from Strings. It can also already
    // 'walk' the ParseTree to create ASTs.
    // * It is an AST provider in that it will notify watchers
    // when ASTs are ready.
    // We should split this into two classes.

    public Stack<AstNodeBuilder<? extends AstNode>> builderStack = new Stack<>();
    public TokenSubStream stream;
    public String filename;

    private ParseTreeListener listener;
    private CommonParserContext context = null;
    public DiffGraphBuilder cpg;
    private final List<AntlrParserDriverObserver> observers = new ArrayList<>();

    private Parser antlrParser;

    public AntlrParserDriver() {
        super();
    }

    public abstract ParseTree parseTokenStreamImpl(TokenSubStream tokens);

    public abstract Lexer createLexer(CharStream input);

    public DiffGraphBuilder parseAndWalkFile(String filename, DiffGraphBuilder diffGraph) throws ParserException {
        cpg  = diffGraph;
        handleHiddenTokens(filename);
        TokenSubStream stream = createTokenStreamFromFile(filename);
        initializeContextWithFile(filename, stream);

        ParseTree tree = parseTokenStream(stream);
        walkTree(tree);
        return cpg;
    }

    private void handleHiddenTokens(String filename) {
        CommonTokenStream tokenStream = createStreamOfHiddenTokensFromFile(filename);
        TokenSource tokenSource = tokenStream.getTokenSource();

        while (true){
            Token token = tokenSource.nextToken();
            if (token.getType() == EOF) {
                break;
            }
            if (token.getChannel() != Token.HIDDEN_CHANNEL) {
                continue;
            }
            int line = token.getLine();
            String text = token.getText();
            NewComment commentNode = NewComment.apply().lineNumber(new Some<>(line)).code(text).filename(filename);
            cpg.addNode(commentNode);
        }
    }

    public void parseAndWalkTokenStream(TokenSubStream tokens)
            throws ParserException {
        filename = "";
        stream = tokens;
        ParseTree tree = parseTokenStream(tokens);
        walkTree(tree);
    }


    public ParseTree parseAndWalkString(String input) throws ParserException {
        ParseTree tree = parseString(input);
        walkTree(tree);
        return tree;
    }

    public CompoundStatement getResult() {
        return (CompoundStatement) builderStack.peek().getItem();
    }

    public ParseTree parseString(String input) throws ParserException {
        CharStream inputStream = CharStreams.fromString(input);
        Lexer lex = createLexer(inputStream);
        TokenSubStream tokens = new TokenSubStream(lex);
        ParseTree tree = parseTokenStream(tokens);
        return tree;
    }

    public ParseTree parseTokenStream(TokenSubStream tokens)
            throws ParserException {
        ParseTree returnTree = parseTokenStreamImpl(tokens);
        if (returnTree == null) {
            throw new ParserException();
        }
        return returnTree;
    }

    public void setAntlrParser(Parser parser) {
        antlrParser = parser;
    }

    public Parser getAntlrParser() {
        return antlrParser;
    }

    protected TokenSubStream createTokenStreamFromFile(String filename)
            throws ParserException {

        CharStream input = createInputStreamForFile(filename);
        Lexer lexer = createLexer(input);
        TokenSubStream tokens = new TokenSubStream(lexer);
        return tokens;

    }

    private CharStream createInputStreamForFile(String filename) {

        try {
            return CharStreams.fromFileName(filename);
        } catch (IOException exception) {
            throw new RuntimeException(String.format("Unable to find source file [%s]", filename));
        }

    }

    protected CommonTokenStream createStreamOfHiddenTokensFromFile(String filename) {
        CharStream input = createInputStreamForFile(filename);
        Lexer lexer = createLexer(input);
        return new CommonTokenStream(lexer, Token.HIDDEN_CHANNEL);
    }

    protected void walkTree(ParseTree tree) {
        ParseTreeWalker walker = new ParseTreeWalker();
        walker.walk(getListener(), tree);
    }

    protected void initializeContextWithFile(String filename,
                                             TokenSubStream stream) {
        setContext(new CommonParserContext());
        getContext().filename = filename;
        getContext().stream = stream;
        initializeContext(getContext());
    }

    protected boolean isRecognitionException(RuntimeException ex) {

        return ex.getClass() == ParseCancellationException.class
                && ex.getCause() instanceof RecognitionException;
    }

    protected void setLLStarMode(Parser parser) {
        parser.removeErrorListeners();
        parser.setErrorHandler(new DefaultErrorStrategy());
    }

    protected void setSLLMode(Parser parser) {
        parser.removeErrorListeners();
        parser.setErrorHandler(new BailErrorStrategy());
    }

    public void initializeContext(CommonParserContext context) {
        filename = context.filename;
        stream = context.stream;
    }

    public void setStack(Stack<AstNodeBuilder<? extends AstNode>> aStack) {
        builderStack = aStack;
    }

    // //////////////////

    public void addObserver(AntlrParserDriverObserver observer) {
        observers.add(observer);
    }

    private void notifyObservers(Consumer<AntlrParserDriverObserver> function) {
        for (AntlrParserDriverObserver observer : observers) {
            function.accept(observer);
        }

    }

    public void begin() {
        notifyObserversOfBegin();
    }

    public void end() {
        notifyObserversOfEnd();
    }

    private void notifyObserversOfBegin() {
        notifyObservers(AntlrParserDriverObserver::begin);
    }

    private void notifyObserversOfEnd() {
        notifyObservers(AntlrParserDriverObserver::end);
    }

    public void notifyObserversOfUnitStart(ParserRuleContext ctx) {
        notifyObservers(observer -> observer.startOfUnit(ctx, filename));
    }

    public void notifyObserversOfUnitEnd(ParserRuleContext ctx) {
        notifyObservers(observer -> observer.endOfUnit(ctx, filename));
    }

    public void notifyObserversOfItem(AstNode aItem) {
        notifyObservers(observer -> observer.processItem(aItem, builderStack));
    }

    public ParseTreeListener getListener() {
        return listener;
    }

    public void setListener(ParseTreeListener listener) {
        this.listener = listener;
    }

    public CommonParserContext getContext() {
        return context;
    }

    public void setContext(CommonParserContext context) {
        this.context = context;
    }

}

