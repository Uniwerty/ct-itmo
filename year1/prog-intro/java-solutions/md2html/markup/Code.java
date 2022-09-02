package md2html.markup;

import java.util.List;

public class Code extends AbstractMarkdown implements Markdown {
    public Code(List<Markdown> list) {
        super(list);
    }

    @Override
    protected void addSignM(StringBuilder str) {}

    @Override
    protected void addSignH(StringBuilder str, int level, boolean end) {
        if (end) str.append("</code>");
        else str.append("<code>");
    }
}
