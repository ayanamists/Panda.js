"use client";

import { Listbox, ListboxItem } from '@nextui-org/listbox';
import PostCard from '@/components/PostCard';
import { Post } from '@/contents/cms';

export default function PostList({ posts }: { posts: Post[] }) {
  return (
    <Listbox>
      {posts.map(post => (
        <ListboxItem key={post.id}
          textValue={post.metaData.title}
        >
          <PostCard post={post} />
        </ListboxItem>
      ))}
    </Listbox>
  );
}