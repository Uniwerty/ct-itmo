package info.kgeorgiy.ja.ivchenkov.crawler;

import info.kgeorgiy.java.advanced.crawler.*;

import java.io.IOException;
import java.util.*;
import java.util.concurrent.*;

/**
 * The class for crawling webpages.
 *
 * @author Ivchenkov Dmitrii
 */
public class WebCrawler implements Crawler {
    private final Downloader downloader;
    private final ExecutorService downloaderService;
    private final ExecutorService extractorService;
    private static final int DEFAULT_DEPTH = 1;
    private static final int DEFAULT_DOWNLOADERS_NUMBER = 1;
    private static final int DEFAULT_EXTRACTORS_NUMBER = 1;
    private static final int DEFAULT_PER_HOST = 1;
    private static final double DOWNLOADER_TIME_SCALE = 1;

    /**
     * Constructs the crawler with the specified downloader,
     * downloader threads number, extractor threads number and limitation per host.
     *
     * @param downloader  the {@link Downloader} to be used
     * @param downloaders the number of downloading threads
     * @param extractors  the number of extracting threads
     * @param perHost     the limitation of the number of downloads per host
     */
    public WebCrawler(Downloader downloader, int downloaders, int extractors, int perHost) {
        this.downloader = downloader;
        downloaderService = Executors.newFixedThreadPool(downloaders);
        extractorService = Executors.newFixedThreadPool(extractors);
    }

    /**
     * The main method of the class to download pages.
     * <p>
     * The method has one required argument – the URL string of the page,
     * and 4 optional ones – the depth of crawling, the downloader threads number,
     * the extractor threads number and the limitation per host in the following order:
     * url [depth [downloads [extractors [perHost]]]].
     *
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        if (args == null || args.length < 1 || args.length > 5) {
            System.err.println("Invalid arguments! Please write: url [depth [downloads [extractors [perHost]]]]");
            return;
        }

        try {
            String startUrl = args[0];
            int depth = getArgumentOrDefault(args, 1, DEFAULT_DEPTH);
            int downloaders = getArgumentOrDefault(args, 2, DEFAULT_DOWNLOADERS_NUMBER);
            int extractors = getArgumentOrDefault(args, 3, DEFAULT_EXTRACTORS_NUMBER);
            int perHost = getArgumentOrDefault(args, 4, DEFAULT_PER_HOST);

            try (WebCrawler crawler = new WebCrawler(
                    new CachingDownloader(DOWNLOADER_TIME_SCALE), downloaders, extractors, perHost)) {
                Result result = crawler.download(startUrl, depth);
                if (!result.getDownloaded().isEmpty()) {
                    System.out.println("Successfully downloaded pages:");
                    result.getDownloaded().forEach(System.out::println);
                }
                if (!result.getErrors().isEmpty()) {
                    System.out.println("Received errors:");
                    result.getErrors().forEach((url, e) -> System.out.printf("%s : %s\n", url, e.getMessage()));
                }
            } catch (IOException e) {
                System.err.println("Cannot create a downloader: " + e.getMessage());
            }
        } catch (NumberFormatException e) {
            System.err.println("Invalid argument: " + e.getMessage());
            System.err.println("Please write: url [depth [downloads [extractors [perHost]]]]");
        }
    }

    /**
     * Downloads the page with the specified URL string recursively to the specified depth.
     *
     * @param url   the start <a href="http://tools.ietf.org/html/rfc3986">URL</a>.
     * @param depth the download depth.
     * @return the {@link Result} of crawling {@code url}
     */
    @Override
    public Result download(String url, int depth) {
        final Set<String> downloaded = ConcurrentHashMap.newKeySet();
        final Set<String> visited = ConcurrentHashMap.newKeySet();
        final Map<String, IOException> errors = new ConcurrentHashMap<>();
        Queue<String> queue = new ArrayDeque<>();
        queue.add(url);
        for (int level = 1; level <= depth; level++) {
            final Phaser phaser = new Phaser(1);
            final boolean toContinueCrawling = level < depth;
            final Queue<String> nextLevelQueue = new ConcurrentLinkedQueue<>();
            queue.stream().filter(visited::add).forEach((link) -> {
                phaser.register();
                downloaderService.submit(() -> {
                    try {
                        final Document document = downloader.download(link);
                        downloaded.add(link);
                        if (toContinueCrawling) {
                            phaser.register();
                            extractorService.submit(() -> {
                                try {
                                    nextLevelQueue.addAll(document.extractLinks());
                                } catch (IOException e) {
                                    updateExceptions(errors, link, e);
                                } finally {
                                    phaser.arrive();
                                }
                            });
                        }
                    } catch (IOException e) {
                        updateExceptions(errors, link, e);
                    } finally {
                        phaser.arrive();
                    }
                });
            });
            phaser.arriveAndAwaitAdvance();
            queue = nextLevelQueue;
        }
        return new Result(new ArrayList<>(downloaded), errors);
    }

    /**
     * Closes the crawler and waits for the termination of executing tasks.
     */
    @Override
    public void close() {
        downloaderService.shutdown();
        extractorService.shutdown();
    }

    private static void updateExceptions(final Map<String, IOException> errors,
                                         final String url,
                                         final IOException exception) {
        errors.merge(url, exception, (oldE, newE) -> {
            oldE.addSuppressed(newE);
            return oldE;
        });
    }

    private static int getArgumentOrDefault(String[] args, int index, int defaultValue) throws NumberFormatException {
        return args.length > index ? Integer.parseInt(args[index]) : defaultValue;
    }
}
