interface TableOfContentsProps {
  children: React.ReactNode
}

export default function TableOfContents({ children }: TableOfContentsProps) {
  return (<div className="toc" id="toc">
    {children}
  </div>)
}