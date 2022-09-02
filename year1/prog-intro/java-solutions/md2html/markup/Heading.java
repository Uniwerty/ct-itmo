package md2html.markup;

import java.util.List;

public class Heading extends AbstractMarkdown implements Markdown {
    public Heading(List<Markdown> list) {
        super(list);
    }

    @Override
    protected void addSignM(StringBuilder str) {}

    @Override
    protected void addSignH(StringBuilder str, int level, boolean end) {
        if (end) str.append("</h" + level + ">");
        else str.append("<h" + level + ">");
    }

}
