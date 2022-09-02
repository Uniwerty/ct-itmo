package markup;

import java.util.List;

public class Emphasis extends AbstractMarkdown implements Markdown{
    public Emphasis(List<Markdown> list){
        super(list);
    }

    @Override
    protected void addSignM(StringBuilder str) {
        str.append("*");
    }

    @Override
    protected void addSignH(StringBuilder str, boolean end) {
        if (end) str.append("</em>");
	else str.append("<em>");
    }
}
