interface QuoteCardProps {
  text: string;
  author: string;
  from: string;
  originURL?: string;
  avatarURL?: string;
}

export default function QuoteCard(props: QuoteCardProps) {
  return (<div className="w-full font-mainpage">
    <div className="rounded-xl border border-foreground/[0.06] bg-foreground/[0.02] font-mainpage">
      <div className="p-4">
        <div className="grid grid-cols-1 sm:grid-cols-3
        grid-rows-2 sm:grid-rows-1
        gap-0 sm:gap-6
        place-items-center">
          { props.avatarURL && <img
            width={200}
            className="max-h-52 rounded-md object-cover"
            alt={`Figure of ${props.author}`}
            src={props.avatarURL}
          />
          }

          <div className="sm:col-span-2">
            <p className="text-lg text-foreground/80 leading-relaxed italic">{props.text}</p>
            <hr className="mx-auto mt-3 border-foreground/10"></hr>
            <p className="mt-2 text-right text-sm text-foreground/60">{props.author},</p>
            <p className="text-right text-sm">
              <span className="italic">
                <a className="text-foreground/50 hover:text-primary transition-colors duration-200" href={props.originURL}>{props.from}</a>
              </span>
            </p>
          </div>
        </div>
      </div>
    </div>
  </div>);
}
