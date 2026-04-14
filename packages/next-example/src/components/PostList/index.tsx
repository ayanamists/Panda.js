"use client";

import PostCard from '@/components/PostCard';
import PostDate from "@/components/PostDate";
import { Post } from '@/contents/cms';
import { getAnimateTitleId } from '@/utils';
import { Link } from '@/navigation';
import { motion } from "framer-motion";

export default function PostList({ posts }: { posts: Post[] }) {
  return (
    <ul className="mt-8 space-y-0 divide-y divide-foreground/[0.06]">
      {posts.map(post => (
        <li key={post.id}>
          <Link
            href={`/posts/${post.id}` as "/"}
            className="group block py-4 -mx-3 px-3 rounded-md
              hover:bg-foreground/[0.03] transition-colors duration-200"
          >
            <motion.div layoutId={getAnimateTitleId(post.id)}>
              <PostCard post={post} />
            </motion.div>
            <div className="mt-1">
              <PostDate date={post.metaData.date} />
            </div>
          </Link>
        </li>
      ))}
    </ul>
  );
}
