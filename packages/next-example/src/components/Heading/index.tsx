import Link from "next/link";

interface HeadingProps {
  level: number;
  children: React.ReactNode;
  id: string;
}

export default function Heading({ children, level, id }: HeadingProps) {
  if (level < 1) {
    level = 1;
  } else if (level > 6) {
    level = 6;
  }

  let fontSize = 'text-4xl';
  if (level === 2) {
    fontSize = 'text-3xl';
  } else if (level === 3) {
    fontSize = 'text-2xl';
  } else if (level === 4) {
    fontSize = 'text-xl';
  } else if (level === 5) {
    fontSize = 'text-lg';
  } else if (level === 6) {
    fontSize = 'text-base';
  }

  const Tag = `h${level}` as keyof JSX.IntrinsicElements;

  return (
    // disable underlining
    <Tag className={`${fontSize}`} id={id}>
      <Link href={`#${id}`} color="secondary" className="pr-3 no-underline hover:underline">#</Link>
      {children}
    </Tag>
  );
}
