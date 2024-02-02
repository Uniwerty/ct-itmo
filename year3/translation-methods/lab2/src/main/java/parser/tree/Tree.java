package parser.tree;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class Tree {
    private static int nodeIndex = 0;
    private final String element;
    private final List<Tree> children;

    public Tree(String element) {
        this(element, new ArrayList<>());
    }

    public Tree(String element, List<Tree> children) {
        this.element = element;
        this.children = children;
    }

    public Tree addChild(Tree child) {
        children.add(child);
        return this;
    }

    @Override
    public String toString() {
        if (children.isEmpty()) {
            return element;
        }
        return String.format("(%s %s)", element, children);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Tree that) {
            return Objects.equals(element, that.element) && Objects.deepEquals(children, that.children);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return element.hashCode() + children.hashCode();
    }

    public String toGraphViz() {
        return toGraphViz(getNextIndex());
    }

    private String toGraphViz(int index) {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append(String.format("%d [label = \"%s\"]%n", index, element));
        for (Tree tree : children) {
            int childIndex = getNextIndex();
            stringBuilder
                    .append(String.format("%d -> %d%n", index, childIndex))
                    .append(tree.toGraphViz(childIndex));
        }
        return stringBuilder.toString();
    }

    private static int getNextIndex() {
        return nodeIndex++;
    }
}
