package edu.arizona.sista.vlsa.demos;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.document.StringField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.index.IndexWriterConfig.OpenMode;
import org.apache.lucene.queryparser.classic.ParseException;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TopScoreDocCollector;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.util.Version;

import java.io.*;
import java.util.Date;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.GZIPInputStream;

class LuceneDemo {

	public static void main(String[] args) throws IOException, ParseException {

        // NOTE: Change to the right corpus and index paths
		String indexPath = "/path/to/data/text/indexes/Gigaword/";
        String corpusPath = "/path/to/data/text/corpora/Gigaword/data/";
		
		// Index corpus
		IndexCorpus.index(corpusPath, indexPath);
		
		// Search indexed corpus
		String queryString = "\"chase\" AND \"run\"";
		ScoreDoc[] hits = SearchIndex.search(indexPath, queryString);
		System.out.println("Found " + hits.length + " documents.");
		
	}

}


/**
 * Search an index.  Adapted from Lucene's demo.
 */
class SearchIndex {

	public static ScoreDoc[] search(String indexPath, String queryString) throws ParseException, IOException {
		
		Analyzer analyzer = new StandardAnalyzer(Version.LUCENE_42);
		QueryParser parser = new QueryParser(Version.LUCENE_42, "text", analyzer);
		Query q = parser.parse(queryString);
		
		int hitsPerPage = 1000000;
		IndexReader reader = DirectoryReader.open(FSDirectory.open(new File(indexPath)));
		IndexSearcher searcher = new IndexSearcher(reader);
		TopScoreDocCollector collector = TopScoreDocCollector.create(hitsPerPage, true);
		searcher.search(q, collector);
		ScoreDoc[] hits = collector.topDocs().scoreDocs;

		// display
		int count = (hits.length > 10) ? 10 : hits.length;
		System.out.println("Displaying top " + count + " documents");
		for (int i = 0; i < count; ++i) {
			int docId = hits[i].doc;
			Document d = searcher.doc(docId);
			System.out.println((i + 1) + ". " + d.get("id"));
		}
		reader.close();
		
		return hits;
	}
	
}


/**
 * Indexing a corpus.  Adapted from lucene's demo.
 */
class IndexCorpus {

	@SuppressWarnings("ConstantConditions")
    public static void index(String corpusPath, String indexPath) throws IOException, NullPointerException {
		
		Date start = new Date();
		
		// Index to directory and not RAM
		Directory dir = FSDirectory.open(new File(indexPath));
		
		// Analyzer performs the task of tokenization plus other things
		Analyzer analyzer = new StandardAnalyzer(Version.LUCENE_42);
		
		// Instantiate index writer
		IndexWriterConfig iwc = new IndexWriterConfig(Version.LUCENE_42, analyzer);
		iwc.setOpenMode(OpenMode.CREATE);
		iwc.setRAMBufferSizeMB(1536.0);		// Need enough heap space (e.g. -Xmx2048m)
		IndexWriter indexWriter = new IndexWriter(dir, iwc);
		
		// Iterate over each source
		File corpusDir = new File(corpusPath);
		for (File sourceDir : corpusDir.listFiles()) {
			if (sourceDir.isDirectory() && sourceDir.getName().endsWith("eng")) {
				for (File file : sourceDir.listFiles()) {
					if (file.isFile() && file.getName().endsWith(".gz"))
						indexFile(indexWriter, file);
				}
			}
		}
		
		// Merge into a single segment for optimization
		indexWriter.forceMerge(1);
		
		// Close the writer
		indexWriter.close();
		
		System.out.println(new Date().getTime() - start.getTime() + " total milliseconds");
	}

	private static void indexFile(IndexWriter indexWriter, File file)
			throws IOException, NullPointerException {

		Pattern idPattern = Pattern.compile("id ?=\"([^\"]*)\"");

		// Open the gzip file, decodes, and feeds into a buffered reader
		InputStream gzipStream = new GZIPInputStream(new FileInputStream(file));
		Reader decoder = new InputStreamReader(gzipStream);
		BufferedReader reader = new BufferedReader(decoder);

		String line;
		while ((line = reader.readLine()) != null) {

			// Skip until the start of a new article
			if (!line.startsWith("<DOC "))
				continue;

			// Skip non-story articles
            if (!line.contains("type=\"story"))
				while ((line = reader.readLine()) != null)
					if (line.startsWith("</DOC>"))
						break;

			// Create a new empty doc for the index writer
			Document doc = new Document();

			// Retrieve the article's unique ID
			String id = "";
			Matcher m = idPattern.matcher(line);
			if (m.find()) {
				id = m.group(1);
			}

			// Add id field to doc (indexed, but not tokenized)
			doc.add(new StringField("id", id, Field.Store.YES));

			// Skip to the <TEXT> tag for the article
            assert line != null;
            while (!line.startsWith("<TEXT>")) {
				if (null == (line = reader.readLine())) {
					reader.close();
					return;
				}
			}

			// Start reading the text
			if (line.startsWith("<TEXT>")) {

				// Read the text for the doc
				StringBuilder sb = new StringBuilder();
				boolean continuing = false;
				while ((line = reader.readLine()) != null) {
					if (line.startsWith("</TEXT>")) {
						// We've reached the end of the text contents, add the text
						// contents to the doc (indexed, tokenized, but not
						// stored).
						doc.add(new TextField("text", sb.toString().trim(),
								Field.Store.NO));
						indexWriter.addDocument(doc);
						break;
					}
					if (line.startsWith("<P>")) {
						continuing = false;
						sb.append("\t ");
					} else if (!line.startsWith("</P>")) {
						if (continuing)
							sb.append(' ');
						else
							continuing = true;
						sb.append(line.indexOf('&') >= 0 ? removeEscapes(line)
								: line);
					}
				}
			}
		}

		// Clean-up
		reader.close();
		decoder.close();
		gzipStream.close();
	}

	private static String removeEscapes(String line) {
		return line.replaceAll("&(amp|AMP);", "&").replaceAll("&(lt|LG);", "<")
				.replaceAll("&(gt|GT);", ">");
	}
}
