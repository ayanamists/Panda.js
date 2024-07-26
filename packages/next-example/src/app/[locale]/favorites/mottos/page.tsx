import QuoteCard from "@/components/QuoteCard";
import { useTranslations } from "next-intl";
import { unstable_setRequestLocale } from "next-intl/server";

const quotes = [{
  text: `It is not only the violin that shapes the violinist, we are all shaped by the tools we train ourselves to use, and in this respect programming languages have a devious influence: they shape our thinking habits.`,
  author: "Edsger W.Dijkstra",
  from: "To the members of the Budget Council",
  originURL: "https://www.cs.utexas.edu/users/EWD/transcriptions/OtherDocs/Haskell.html",
  avatarURL: "https://upload.wikimedia.org/wikipedia/commons/d/d9/Edsger_Wybe_Dijkstra.jpg"
}, {
  text: "Young man, in mathematics you don't understand things. You just get used to them.",  
  author: "John Von Neumann",
  from: "Quoted from The Dancing Wu Li Masters: An Overview of the New Physics. By Gary Zukav",
  originURL: "https://todayinsci.com/V/VonNeumann_John/VonNeumannJohn-UnderstandQuote500px.htm",
  avatarURL: "https://www.ias.edu/sites/default/files/styles/portrait/public/images/scholars/6647.jpg?itok=Tq-b7DF5"
}, {
  text: "Stop reading when you get bored, or the material gets too complicated, or too much specialist knowledge is needed, or the writing is bad.",
  author: "Richard Bird",
  from: "Advice to the reviewers of the Functional Pearls",
  originURL: "https://patternsinfp.wordpress.com/2022/06/06/richard-bird/",
  avatarURL: "https://patternsinfp.wordpress.com/wp-content/uploads/2022/06/bird.jpg?w=1450"
}, {
  text: "わたし、気になります！",
  author: "千反田えるが",
  from: "米沢穂信『〈古典部〉シリーズ』",
  originURL: "https://dic.pixiv.net/a/%E3%82%8F%E3%81%9F%E3%81%97%E3%80%81%E6%B0%97%E3%81%AB%E3%81%AA%E3%82%8A%E3%81%BE%E3%81%99",
  avatarURL: "https://livedoor.blogimg.jp/acideigakan/imgs/f/5/f5af7021.jpg"
}]

export default function Favorites({ params }: {
  params: { locale: string }
}) {
  
  unstable_setRequestLocale(params.locale);
  const t = useTranslations("Favorites");
  return (<main className="w-full md:w-2xl lg:w-3xl mx-auto">
    <h1 className="text-4xl text-center">
      {t("mottos")}
    </h1>

    <div className="flex flex-col flex-wrap gap-2 content-center mt-4">
      {quotes.map((q, id) => <QuoteCard key={id} {...q} /> )}
    </div>
  </main>);
}
