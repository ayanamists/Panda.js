"use client";

import { Listbox, ListboxItem } from "@heroui/listbox";
import PostCard from '@/components/PostCard';
import PostDate from "@/components/PostDate";
import { Post } from '@/contents/cms';
import { getAnimateTitleId } from '@/utils';
import { Link } from '@/navigation';
import { motion } from "framer-motion";

export default function PostList({ posts }: { posts: Post[] }) {
  return (
    <Listbox aria-label='Post List' className='mt-6'>
      {posts.map(post => (
        <ListboxItem
          key={post.id}
          href={`/posts/${post.id}`}
          as={Link}
          description={<PostDate date={post.metaData.date} />}
          textValue={post.metaData.title}
        >
          <motion.div
            key={post.id}
            layoutId={getAnimateTitleId(post.id)}
          >
            <PostCard post={post} />
          </motion.div>
        </ListboxItem>
      ))}
    </Listbox>
  );
}

