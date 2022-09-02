package markup;

import java.util.List;

public class Strong extends AbstractMarkdown implements Markdown{
    public Strong(List<Markdown> list){
        super(list);
    }

    @Override
    protected void addSignM(StringBuilder str) {
        str.append("__");
    }

    @Override
    protected void addSignH(StringBuilder str, boolean end) {
        if (end) str.append("</strong>");
	else str.append("<strong>");
    }
}
