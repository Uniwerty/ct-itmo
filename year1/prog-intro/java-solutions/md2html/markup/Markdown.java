package md2html.markup;

public interface Markdown {
    void toMarkdown(StringBuilder str);

    void toHtml(StringBuilder str, int level);
}
