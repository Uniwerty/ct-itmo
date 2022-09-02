import java.io.*;
import java.util.*;
import java.lang.StringBuilder;
import java.lang.Character;

class MyScanner {
    private Reader scanner;
    private char[] buffer = new char[1024];
    private int cur = 0;
    private int index = 0;

    public MyScanner(InputStream source) {
	this.scanner = new InputStreamReader(source);
    }

    public MyScanner(String source) {
	this.scanner = new StringReader(source);
    }

    public MyScanner(File source, String code) throws IOException {
	this.scanner = new InputStreamReader(new FileInputStream(source), code);
    }

    private void readBuffer() throws IOException {
	cur = scanner.read(buffer);
	index = 0;
    }

    public boolean hasNextLine() throws IOException {
	if (index == cur) this.readBuffer();
	return cur != -1;
    }

    public boolean hasNext() throws IOException {
	while (true) {
	    while (index < cur && Character.isWhitespace(buffer[index])) {
		index++;
		if (index == cur) {
		    this.readBuffer();
	        }
	    }
	    if (index == cur) {
		this.readBuffer();
		continue;
	    }
	    if (cur == -1) {
		return false;
	    }
	    if (index < cur) {
		return true;
	    }
	}
    }

    public String nextLine() throws IOException {
	StringBuilder line = new StringBuilder();
	while (index < cur && (System.lineSeparator().length() == 1 && buffer[index] != '\n' && buffer[index] != '\r' ||
			System.lineSeparator().length() != 1 && buffer[index] != '\n')) {
	    line.append(buffer[index]);
	    index++;
	    if (index == cur) {
			this.readBuffer();
	    }
	}
	index++;
	return line.toString();
    }

    public String next() throws IOException {
	StringBuilder line = new StringBuilder();
	while (true) {
	    if (line.length() == 0) {
	        while (index < cur && Character.isWhitespace(buffer[index])) {
		    index++;
	        }
	    }
	    while (index < cur && !Character.isWhitespace(buffer[index])) {
		line.append(buffer[index]);
		index++;
	    }
	    if (index == cur) {
		this.readBuffer();
		continue;
	    }
	    if (cur == -1 || index < cur) {
		break;
	    }
	}
	return line.toString();
    }

    public void close() throws IOException {
	scanner.close();
    }
}