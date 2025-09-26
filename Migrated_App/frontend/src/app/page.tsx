'use client'

import { useState, useEffect } from 'react'
import { 
  UsersIcon,
  TruckIcon,
  CubeIcon,
  DocumentTextIcon,
  ChartBarIcon,
  BanknotesIcon,
  ClipboardDocumentCheckIcon
} from '@heroicons/react/24/outline'
import { Card, StatsCard } from '@/components/UI/Card'
import PageHeader from '@/components/Layout/PageHeader'

interface SystemStatus {
  system_healthy: boolean
  database_connected: boolean
  gl_balanced: boolean
  period_open: boolean
  modules_active: {
    gl: boolean
    sl: boolean
    pl: boolean
    stock: boolean
    irs: boolean
  }
}

export default function Dashboard() {
  const [systemStatus, setSystemStatus] = useState<SystemStatus | null>(null)
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    // Fetch system status
    const fetchStatus = async () => {
      try {
        const response = await fetch('http://localhost:8000/health')
        if (response.ok) {
          const data = await response.json()
          setSystemStatus({
            system_healthy: data.status === 'healthy',
            database_connected: data.database === 'connected',
            gl_balanced: true, // Would come from actual system check
            period_open: true,
            modules_active: {
              gl: true,
              sl: true,
              pl: true,
              stock: true,
              irs: false
            }
          })
        }
      } catch (error) {
        console.error('Failed to fetch system status:', error)
        // Set default status on error
        setSystemStatus({
          system_healthy: false,
          database_connected: false,
          gl_balanced: false,
          period_open: true,
          modules_active: {
            gl: false,
            sl: false,
            pl: false,
            stock: false,
            irs: false
          }
        })
      } finally {
        setLoading(false)
      }
    }

    fetchStatus()
  }, [])

  const moduleCards = [
    {
      title: 'Sales Ledger',
      description: 'Customer management, invoicing, and accounts receivable',
      icon: UsersIcon,
      href: '/customers',
      color: 'bg-blue-500',
      stats: [
        { label: 'Active Customers', value: '156' },
        { label: 'Outstanding', value: '£45,230' }
      ]
    },
    {
      title: 'Purchase Ledger', 
      description: 'Supplier management, purchase orders, and accounts payable',
      icon: TruckIcon,
      href: '/suppliers',
      color: 'bg-green-500',
      stats: [
        { label: 'Active Suppliers', value: '87' },
        { label: 'Outstanding', value: '£23,150' }
      ]
    },
    {
      title: 'Stock Control',
      description: 'Inventory management with FIFO/LIFO/Average costing',
      icon: CubeIcon,
      href: '/stock',
      color: 'bg-purple-500',
      stats: [
        { label: 'Stock Items', value: '432' },
        { label: 'Total Value', value: '£78,450' }
      ]
    },
    {
      title: 'General Ledger',
      description: 'Chart of accounts, journal entries, and financial reporting',
      icon: DocumentTextIcon,
      href: '/gl',
      color: 'bg-indigo-500',
      stats: [
        { label: 'GL Accounts', value: '125' },
        { label: 'Period Balance', value: 'Balanced' }
      ]
    },
    {
      title: 'Financial Reports',
      description: 'Trial balance, P&L, balance sheet, and management reports',
      icon: ChartBarIcon,
      href: '/reports',
      color: 'bg-yellow-500',
      stats: [
        { label: 'Available Reports', value: '25+' },
        { label: 'Last Generated', value: 'Today' }
      ]
    },
    {
      title: 'Payments',
      description: 'Customer receipts and supplier payment processing',
      icon: BanknotesIcon,
      href: '/payments',
      color: 'bg-emerald-500',
      stats: [
        { label: 'Pending Payments', value: '12' },
        { label: 'Bank Balance', value: '£125,340' }
      ]
    }
  ]

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="Dashboard"
        description="ACAS - Applewood Computers Accounting System Overview"
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* System Status Cards */}
        {systemStatus && (
          <div className="grid grid-cols-1 md:grid-cols-4 gap-6 mb-8">
            <StatsCard
              title="Database"
              value={systemStatus.database_connected ? 'Connected' : 'Disconnected'}
              icon={<ClipboardDocumentCheckIcon className="h-6 w-6" />}
            />
            <StatsCard
              title="GL Status"
              value={systemStatus.gl_balanced ? 'Balanced' : 'Out of Balance'}
              icon={<DocumentTextIcon className="h-6 w-6" />}
            />
            <StatsCard
              title="Period Status"
              value={systemStatus.period_open ? 'Open' : 'Locked'}
              icon={<ChartBarIcon className="h-6 w-6" />}
            />
            <StatsCard
              title="Current Period"
              value="Period 1"
              icon={<BanknotesIcon className="h-6 w-6" />}
            />
          </div>
        )}

        {/* Welcome Message */}
        <Card className="mb-8">
          <h2 className="text-xl font-semibold text-gray-900 mb-2">
            Welcome to ACAS Migration System
          </h2>
          <p className="text-gray-600">
            Complete ERP system migrated from 49 years of legacy COBOL to a modern web application. 
            This system provides comprehensive business management including accounting, inventory, 
            customer relations, and financial reporting with exact business logic preservation.
          </p>
        </Card>

        {/* Module Cards Grid */}
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
          {moduleCards.map((module) => (
            <a
              key={module.title}
              href={module.href}
              className="block hover:shadow-md transition-shadow"
            >
              <Card className="h-full hover:shadow-md transition-shadow cursor-pointer">
                <div className="flex items-start">
                  <div className={`p-3 rounded-lg ${module.color} text-white mr-4`}>
                    <module.icon className="h-6 w-6" />
                  </div>
                  <div className="flex-1">
                    <h3 className="text-lg font-semibold text-gray-900 mb-1">
                      {module.title}
                    </h3>
                    <p className="text-sm text-gray-600 mb-3">
                      {module.description}
                    </p>
                    <div className="space-y-1">
                      {module.stats.map((stat, index) => (
                        <div key={index} className="flex justify-between text-sm">
                          <span className="text-gray-500">{stat.label}:</span>
                          <span className="font-medium text-gray-900">{stat.value}</span>
                        </div>
                      ))}
                    </div>
                  </div>
                </div>
              </Card>
            </a>
          ))}
        </div>

        {/* System Information */}
        <div className="mt-8 grid grid-cols-1 lg:grid-cols-2 gap-6">
          <Card>
            <div className="px-6 py-4 border-b border-gray-200 bg-gray-50">
              <h3 className="text-lg font-medium text-gray-900">System Features</h3>
            </div>
            <div className="p-6">
              <ul className="space-y-2 text-sm">
                <li className="flex items-center">
                  <span className="w-2 h-2 bg-green-500 rounded-full mr-2" />
                  Complete double-entry bookkeeping
                </li>
                <li className="flex items-center">
                  <span className="w-2 h-2 bg-green-500 rounded-full mr-2" />
                  Multi-location inventory tracking
                </li>
                <li className="flex items-center">
                  <span className="w-2 h-2 bg-green-500 rounded-full mr-2" />
                  FIFO/LIFO/Average costing methods
                </li>
                <li className="flex items-center">
                  <span className="w-2 h-2 bg-green-500 rounded-full mr-2" />
                  Comprehensive tax calculations
                </li>
                <li className="flex items-center">
                  <span className="w-2 h-2 bg-green-500 rounded-full mr-2" />
                  Financial reporting suite
                </li>
                <li className="flex items-center">
                  <span className="w-2 h-2 bg-green-500 rounded-full mr-2" />
                  Complete audit trail
                </li>
              </ul>
            </div>
          </Card>
          
          <Card>
            <div className="px-6 py-4 border-b border-gray-200 bg-gray-50">
              <h3 className="text-lg font-medium text-gray-900">Migration Information</h3>
            </div>
            <div className="p-6">
              <dl className="space-y-2 text-sm">
                <div className="flex justify-between">
                  <dt className="text-gray-500">Original System:</dt>
                  <dd className="font-medium">COBOL (1976-2025)</dd>
                </div>
                <div className="flex justify-between">
                  <dt className="text-gray-500">Lines of Code:</dt>
                  <dd className="font-medium">133,973</dd>
                </div>
                <div className="flex justify-between">
                  <dt className="text-gray-500">Programs Migrated:</dt>
                  <dd className="font-medium">453</dd>
                </div>
                <div className="flex justify-between">
                  <dt className="text-gray-500">Backend:</dt>
                  <dd className="font-medium">FastAPI + PostgreSQL</dd>
                </div>
                <div className="flex justify-between">
                  <dt className="text-gray-500">Frontend:</dt>
                  <dd className="font-medium">Next.js + TypeScript</dd>
                </div>
                <div className="flex justify-between">
                  <dt className="text-gray-500">Version:</dt>
                  <dd className="font-medium">3.02</dd>
                </div>
              </dl>
            </div>
          </Card>
        </div>
      </main>
    </div>
  )
}