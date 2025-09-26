import type { Metadata } from 'next'
import { Inter } from 'next/font/google'
import './globals.css'
import Navbar from '@/components/Layout/Navbar'

const inter = Inter({ subsets: ['latin'] })

export const metadata: Metadata = {
  title: 'ACAS - Applewood Computers Accounting System',
  description: 'Complete ERP system migrated from legacy COBOL to modern web application',
  keywords: 'accounting, ERP, business management, invoicing, inventory, financial reporting',
}

export default function RootLayout({
  children,
}: {
  children: React.ReactNode
}) {
  return (
    <html lang="en">
      <body className={inter.className}>
        <div className="min-h-screen bg-gray-50">
          <Navbar />
          <div className="lg:pl-72">
            {children}
          </div>
        </div>
      </body>
    </html>
  )
}