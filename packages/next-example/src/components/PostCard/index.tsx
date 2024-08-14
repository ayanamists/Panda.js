"use client";

import { Post } from '@/contents/cms';
import { Card, CardBody, CardFooter } from "@nextui-org/card";
import Link from 'next/link';

export default function PostCard({ post }: { post: Post }) {
  return (
    <Card disableAnimation={true} isHoverable={false} fullWidth={true}
      disableRipple={true} className='text-foreground bg-background'
      shadow="none"
    >
      <CardBody>
        <Link href={`/${post.metaData.language}/posts/${post.id}`}
          className='text-lg whitespace-normal text-primary'
          aria-label='Post'
        >
          {post.metaData.title}
        </Link>
      </CardBody>
      <CardFooter className="flex flex-row justify-between">
        <p className="text-md">{new Date(post.metaData.date).toLocaleDateString()}</p>
        <p className="text-small text-default-500">{post.metaData.categories.join(" ")}</p>
      </CardFooter>
    </Card>
  );
}
