import { Card, CardBody } from "@nextui-org/card";
import { Image } from "@nextui-org/image";

interface QuoteCardProps {
  text: string;
  author: string;
  from: string;
  originURL?: string;
  avatarURL?: string;
}

export default function QuoteCard(props: QuoteCardProps) {
  return (<div className="xl:w-2/5 md:max-lg:w-4/5 w-4/5 font-post">
    <Card
      isBlurred
      shadow="sm"
    >
      <CardBody>
        <div className="grid grid-cols-1 sm:grid-cols-3
        grid-rows-2 sm:grid-rows-1
        gap-0 sm:gap-4
        place-items-center">
          { props.avatarURL && <Image
            width={"200"}
            className="max-h-200"
            alt={`Figure of ${props.author}`}
            shadow="sm"
            src={props.avatarURL}
          />
          }

          <div className="sm:col-span-2">
            <p className="text-xl">{props.text}</p>
            <hr className="mx-auto mt-2 border-primary"></hr>
            <p className="mt-2 text-right">{props.author},</p>
            <p className="text-right">
              <span className="italic">
                <a className="hover:underline" href={props.originURL}>{props.from}</a>
              </span>
            </p>
          </div>
        </div>
      </CardBody>
    </Card>
  </div>);
}
