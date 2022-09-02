package markup;

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
    public void toHtml(StringBuilder str) {
        addSignH(str, false);
        for (Markdown markdown : list) {
            markdown.toHtml(str);
        }
        addSignH(str, true);
    }

    protected abstract void addSignM(StringBuilder str);
    protected abstract void addSignH(StringBuilder str, boolean end);
}
