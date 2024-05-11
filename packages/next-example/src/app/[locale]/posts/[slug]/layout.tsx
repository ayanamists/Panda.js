export default function Container({ children }: { children: React.ReactNode }) {
  return (
    <div className="
    relative w-full px-6 py-12 shadow-xl 
    shadow-slate-700/20 ring-1 ring-gray-900/5
    dark:shadow-slate-300/20 dark:ring-gray-100/5 dark:shadow-slate-900/10
    md:max-w-3xl md:mx-auto lg:max-w-4xl lg:pt-16 lg:pb-28
    prose prose-natural dark:prose-invert">
      {children}
    </div>
  )
}