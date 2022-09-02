package md2html.markup;

import java.util.List;

public abstract class AbstractMarkdown implements Markdown{
    protected List<Markdown> list;

    public AbstractMarkdown(List<Markdown> list) {
        this.list = list;
    }

    @Override
    public void toMarkdown(StringBuilder str) {
        addSignM(str);
        for (Markdown markdown : list) {
            markdown.toMarkdown(str);
        }
        addSignM(str);
    }

    @Override
    public void toHtml(StringBuilder str, int level) {
        addSignH(str, level, false);
        for (Markdown markdown : list) {
            markdown.toHtml(str, 0);
        }
        addSignH(str, level, true);
    }

    protected abstract void addSignM(StringBuilder str);
    protected abstract void addSignH(StringBuilder str, int level, boolean end);
}
