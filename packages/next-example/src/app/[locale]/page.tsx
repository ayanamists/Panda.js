import { FaGithub } from "react-icons/fa";
import { FaTwitter } from "react-icons/fa";
import { FaHeart } from "react-icons/fa";
import { SiDouban } from "react-icons/si";
import { SiZhihu } from "react-icons/si";
import { SiMeituan } from "react-icons/si";

import React from 'react';
import Link from 'next/link';

function SocialLink({ href, icon, label }: {
  href: string,
  icon: React.ReactNode,
  label: string
}) {
  return (
    <Link
      href={href}
      aria-label={label}
      className="text-foreground/50 hover:text-primary transition-colors duration-300"
    >
      <span className="text-lg">{icon}</span>
    </Link>
  )
}

function SectionRule({ delay }: { delay: string }) {
  return (
    <div className={`animate-rule-expand ${delay} my-5`}>
      <div className="h-px bg-foreground/10" />
    </div>
  )
}

function SectionLabel({ children, delay }: { children: React.ReactNode, delay: string }) {
  return (
    <div className={`animate-fade-up ${delay} flex items-baseline gap-2.5 mb-2.5`}>
      <span className="text-primary font-heading text-base">§</span>
      <h2 className="text-sm font-heading tracking-[0.2em] uppercase text-foreground/50">
        {children}
      </h2>
    </div>
  )
}

function HomePage() {
  return (
    <div className="min-h-[calc(100vh-2.5rem)] flex items-center justify-center px-6 md:px-12">
      <div className="w-full max-w-xl py-12 md:py-16">

        {/* ── Identity ── */}
        <header className="mb-4">
          {/* Type judgment — the polymorphic identity */}
          <div className="animate-fade-in delay-0 mb-5 select-none" aria-hidden="true">
            <span className="font-serif italic text-primary/20 tracking-[0.3em] text-base">
              Γ <span className="not-italic text-primary/15">⊢</span> λx.x
              <span className="not-italic text-primary/15 mx-0.5">:</span>
              ∀α. α → α
            </span>
          </div>

          {/* Name */}
          <h1 className="animate-fade-up delay-1 font-mainpage">
            <span className="text-3xl md:text-4xl tracking-[0.12em]">
              李晨曦
            </span>
          </h1>

          {/* IPA + Romanization */}
          <div className="animate-fade-up delay-1 mt-1.5 flex items-baseline gap-2.5">
            <span className="text-base text-foreground/50 tracking-wide font-mainpage">
              Li Chenxi
            </span>
            <span className="text-sm text-foreground/30 tracking-wider font-mono">
              /liˇ ʈʂʰənˊ ɕiˉ/
            </span>
          </div>

          {/* Social row */}
          <div className="animate-fade-up delay-2 mt-3.5 flex items-center gap-3.5">
            <SocialLink href="https://github.com/ayanamists" icon={<FaGithub />} label="GitHub" />
            <SocialLink href="https://x.com/chezchenxi" icon={<FaTwitter />} label="Twitter" />
            <SocialLink href="https://www.douban.com/people/191397140" icon={<SiDouban />} label="Douban" />
            <SocialLink href="https://www.zhihu.com/people/woodwardchenxi" icon={<SiZhihu />} label="Zhihu" />
          </div>
        </header>

        <SectionRule delay="delay-2" />

        {/* ── About ── */}
        <section>
          <SectionLabel delay="delay-3">About</SectionLabel>
          <div className="animate-fade-up delay-3 leading-snug font-mainpage space-y-1.5">
            <p className="text-lg text-foreground/80">
              A programmer working at the intersection of theory and practice.
            </p>
            <p className="text-base text-foreground/60">
              B.Eng., Sun Yat-sen University
              <span className="mx-1.5 text-foreground/25">·</span>
              M.Eng., Nanjing University
            </p>
            <p className="inline-flex items-center text-base text-foreground/60">
              <span>Currently at Meituan</span>
              <SiMeituan className="w-4 h-4 ml-1.5 text-foreground/35" />
              <span className="mx-1.5 text-foreground/25">·</span>
              <span className="inline-flex items-center gap-1 text-red-400/80 dark:text-red-400/60">
                <FaHeart className="w-2.5 h-2.5 animate-pulse" />
                <span className="font-serif italic tracking-wide">In love</span>
              </span>
            </p>
          </div>
        </section>

        <SectionRule delay="delay-4" />

        {/* ── Research & Engineering side-by-side ── */}
        <section className="animate-fade-up delay-4">
          <div className="flex flex-col sm:flex-row gap-6 sm:gap-16">
            <div className="flex-1">
              <SectionLabel delay="delay-4">Research</SectionLabel>
              <ul className="space-y-1">
                {[
                  'Functional Programming',
                  'Programming Languages',
                  'Program Analysis',
                  'Mathematical Logic',
                ].map((item) => (
                  <li key={item} className="text-base text-foreground/70 font-mainpage flex items-baseline gap-2">
                    <span className="text-primary/40 text-sm">—</span>
                    {item}
                  </li>
                ))}
              </ul>
            </div>

            <div className="flex-1">
              <SectionLabel delay="delay-5">Engineering</SectionLabel>
              <ul className="space-y-1">
                {[
                  'Web Development',
                  'Compiler',
                  'Docker & CI/CD',
                  'VSCode / Emacs',
                ].map((item) => (
                  <li key={item} className="text-base text-foreground/70 font-mainpage flex items-baseline gap-2">
                    <span className="text-primary/40 text-sm">—</span>
                    {item}
                  </li>
                ))}
              </ul>
            </div>
          </div>
        </section>

        <SectionRule delay="delay-6" />

        {/* ── Publications ── */}
        <section>
          <SectionLabel delay="delay-6">Publications</SectionLabel>
          <div className="animate-fade-up delay-6 text-base leading-snug text-foreground/70 font-mainpage pl-5 -indent-5">
            <span className="text-primary/50 font-mono text-sm mr-0.5">[1]</span>
            {' '}
            <span className="text-foreground/90">Chenxi Li</span>,
            {' '}Haoran Lin, Tian Tan, and Yue Li.
            {' '}(2025).
            {' '}
            <a
              href="https://doi.org/10.1145/3763081"
              target="_blank"
              rel="noopener noreferrer"
              className="text-foreground/80 underline decoration-foreground/15 underline-offset-2
                hover:text-primary hover:decoration-primary/40 transition-colors duration-300"
            >
              Two Approaches to Fast Bytecode Frontend for Static Analysis.
            </a>
            {' '}
            In <em>Proc. ACM Program. Lang.</em> 9, OOPSLA2,
            Art. 303, 27 pp.
            {' '}
            <a
              href="https://doi.org/10.1145/3763081"
              target="_blank"
              rel="noopener noreferrer"
              className="text-primary/60 font-mono text-sm hover:text-primary transition-colors duration-300"
            >
              doi↗
            </a>
          </div>
        </section>

      </div>
    </div>
  );
}

export default async function Index() {
  return (
    <main>
      <HomePage />
    </main>
  );
}
