package markup;

import java.util.List;

public class Paragraph extends AbstractMarkdown implements Markdown{
    public Paragraph(List<Markdown> list){
        super(list);
    }

    @Override
    protected void addSignM(StringBuilder str) {}

    @Override
    protected void addSignH(StringBuilder str, boolean end) {}
}
