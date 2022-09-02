package md2html.markup;

import java.util.List;

public class Strikeout extends AbstractMarkdown implements Markdown{
    public Strikeout(List<Markdown> list){
        super(list);
    }

    @Override
    protected void addSignM(StringBuilder str) {
        str.append("~");
    }

    @Override
    protected void addSignH(StringBuilder str, int level, boolean end) {
        if (end) str.append("</s>");
	    else str.append("<s>");
    }
}
