export default function Container({ children }: { children: React.ReactNode }) {
  return (
    <div className="
    relative w-full px-6 py-12 shadow-xl 
    shadow-slate-700/20 ring-1 ring-gray-900/5
    dark:shadow-slate-100/20 dark:ring-gray-100/5
    md:max-w-3xl md:mx-auto lg:max-w-4xl lg:pt-16 lg:pb-28
    prose prose-natural dark:prose-invert">
      <div className="px-2">
        {children}
      </div>
    </div>
  )
}