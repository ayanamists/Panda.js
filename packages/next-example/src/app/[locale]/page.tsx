import { useTranslations } from 'next-intl';
import { unstable_setRequestLocale } from 'next-intl/server';
import { FaGithub } from "react-icons/fa";
import { SiDouban } from "react-icons/si";
import { SiZhihu } from "react-icons/si";

import React from 'react';
import Link from 'next/link';

function SmallTitle({ children }: { children: React.ReactNode }) {
  return (<h2 className='text-xl font-bold font-heading'>
    {children}
  </h2>)
}

function ProfileBadge({ href, icon }: {
  href: string,
  icon: React.ReactNode
}) {
  return (<div>
    <Link href={href}>
      {icon}
    </Link>
  </div>)
}

function HomePage() {
  const t = useTranslations('Index');

  return (
    <div className='flex min-h-[60vh] ml-6 mr-6'>
      <div className='m-auto flex gap-10 flex-wrap
        w-full md:w-3xl md:mx-auto lg:w-4xl justify-center'>
        <div className='w-full md:w-48 lg:w-30 flex flex-col'>
          <div className='text-3xl text-center font-mainpage'>
            <ruby>李晨曦<rt>liˇ ʈʂʰənˊ ɕiˉ</rt></ruby>
            <div className='text-sm'>Li Chenxi</div>
          </div>
          <div className='flex mt-2 gap-1 text-2xl justify-center'>
            <ProfileBadge href={"https://github.com/ayanamists"} icon={<FaGithub />} />
            <ProfileBadge href={"https://www.douban.com/people/191397140"} icon={<SiDouban />} />
            <ProfileBadge href={"https://www.zhihu.com/people/woodwardchenxi"} icon={<SiZhihu />} />
          </div>
        </div>

        <div className='md:max-w-xl lg:max-w-2xl font-mainpage'>
          <div className='mb-5'>
            <SmallTitle>About</SmallTitle>
            {`I am currently a second-year master's student at Nanjing University, majoring in Computer Science. 
            `}
          </div>

          <div className='flex lg:gap-20 md:gap-20 gap-5 flex-wrap'>
            <div>
              <SmallTitle>Interest Fields</SmallTitle>
              <ul>
                <li>Functional Programming</li>
                <li>Programming Languages (Theory)</li>
                <li>Program Analysis</li>
                <li>Mathematical Logic</li>
              </ul>
            </div>

            <div>
              <SmallTitle>Dev Skills</SmallTitle>
              <ul>
                <li>Web Development</li>
                <li>Compiler</li>
                <li>Docker</li>
                <li>CI/CD</li>
                <li>VSCode / Emacs</li>
              </ul>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}

export default function Index({ params }: {
  params: { locale: string }
}) {
  unstable_setRequestLocale(params.locale);

  return (
    <main>
      <HomePage />
    </main>);
}
