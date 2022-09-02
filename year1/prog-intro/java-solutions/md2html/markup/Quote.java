package md2html.markup;

import java.util.List;

public class Quote extends AbstractMarkdown implements Markdown {
    public Quote(List<Markdown> list) {
        super(list);
    }

    @Override
    protected void addSignM(StringBuilder str) {}

    @Override
    protected void addSignH(StringBuilder str, int level, boolean end) {
        if (end) str.append("</q>");
        else str.append("<q>");
    }
}
