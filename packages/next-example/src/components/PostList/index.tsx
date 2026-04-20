"use client";

import PostDate from "@/components/PostDate";
import { Post } from '@/contents/cms';
import { getAnimateTitleId } from '@/utils';
import { Link } from '@/navigation';
import { motion } from "framer-motion";

function groupByYear(posts: Post[]): [string, Post[]][] {
  const groups = new Map<string, Post[]>();
  for (const post of posts) {
    const year = new Date(post.metaData.date).getFullYear().toString();
    const group = groups.get(year) ?? [];
    group.push(post);
    groups.set(year, group);
  }
  return Array.from(groups.entries());
}

export default function PostList({ posts }: { posts: Post[] }) {
  const years = groupByYear(posts);

  return (
    <div className="space-y-12">
      {years.map(([year, yearPosts]) => (
        <section key={year}>
          <h2 className="font-heading text-xs uppercase tracking-[0.2em] text-foreground/25 mb-4">
            {year}
          </h2>
          <ul>
            {yearPosts.map(post => (
              <li key={post.id}>
                <Link
                  href={`/posts/${post.id}` as "/"}
                  className="group flex items-baseline justify-between gap-4
                    py-2.5 transition-colors duration-200"
                >
                  <motion.span
                    layoutId={getAnimateTitleId(post.id)}
                    className="text-[15px] text-foreground/70 group-hover:text-foreground/95
                      transition-colors duration-200 font-mainpage leading-snug"
                  >
                    {post.metaData.title}
                  </motion.span>
                  <span className="shrink-0 hidden sm:block">
                    <PostDate date={post.metaData.date} compact />
                  </span>
                </Link>
              </li>
            ))}
          </ul>
        </section>
      ))}
    </div>
  );
}
