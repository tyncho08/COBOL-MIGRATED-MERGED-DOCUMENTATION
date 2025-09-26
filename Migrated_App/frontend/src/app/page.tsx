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

interface DashboardStats {
  sales: {
    activeCustomers: number
    outstanding: number
  }
  purchase: {
    activeSuppliers: number
    outstanding: number
  }
  stock: {
    totalItems: number
    totalValue: number
  }
  gl: {
    accountsCount: number
    isBalanced: boolean
  }
  reports: {
    availableReports: number
    lastGenerated: string
  }
  payments: {
    pendingCount: number
    bankBalance: number
  }
}

export default function Dashboard() {
  const [systemStatus, setSystemStatus] = useState<SystemStatus | null>(null)
  const [dashboardStats, setDashboardStats] = useState<DashboardStats | null>(null)
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    // Fetch system status and dashboard stats
    const fetchData = async () => {
      try {
        // Fetch system status
        const healthResponse = await fetch('http://localhost:8000/health')
        if (healthResponse.ok) {
          const data = await healthResponse.json()
          setSystemStatus({
            system_healthy: data.status === 'healthy',
            database_connected: data.database === 'connected',
            gl_balanced: true,
            period_open: true,
            modules_active: {
              gl: true,
              sl: true,
              pl: true,
              stock: true,
              irs: true
            }
          })
        }

        // Fetch dashboard statistics from API
        const dashboardResponse = await fetch('http://localhost:8000/api/v1/dashboard/stats')
        if (dashboardResponse.ok) {
          const data = await dashboardResponse.json()
          setDashboardStats({
            sales: {
              activeCustomers: data.sales.activeCustomers,
              outstanding: data.sales.outstanding
            },
            purchase: {
              activeSuppliers: data.purchase.activeSuppliers,
              outstanding: data.purchase.outstanding
            },
            stock: {
              totalItems: data.stock.totalItems,
              totalValue: data.stock.totalValue
            },
            gl: {
              accountsCount: data.gl.accountsCount,
              isBalanced: data.gl.isBalanced
            },
            reports: {
              availableReports: data.reports.availableReports,
              lastGenerated: data.reports.lastGenerated
            },
            payments: {
              pendingCount: data.payments.pendingCount,
              bankBalance: data.payments.bankBalance
            }
          })
        }
      } catch (error) {
        console.error('Failed to fetch dashboard data:', error)
        // Set default values on error
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

    fetchData()
  }, [])

  const moduleCards = [
    {
      title: 'Sales Ledger',
      description: 'Customer management, invoicing, and accounts receivable',
      icon: UsersIcon,
      href: '/customers',
      color: 'bg-blue-500',
      stats: [
        { label: 'Active Customers', value: dashboardStats?.sales.activeCustomers.toString() || '0' },
        { label: 'Outstanding', value: dashboardStats ? `$${dashboardStats.sales.outstanding.toLocaleString()}` : '$0' }
      ]
    },
    {
      title: 'Purchase Ledger', 
      description: 'Supplier management, purchase orders, and accounts payable',
      icon: TruckIcon,
      href: '/suppliers',
      color: 'bg-green-500',
      stats: [
        { label: 'Active Suppliers', value: dashboardStats?.purchase.activeSuppliers.toString() || '0' },
        { label: 'Outstanding', value: dashboardStats ? `$${dashboardStats.purchase.outstanding.toLocaleString()}` : '$0' }
      ]
    },
    {
      title: 'Stock Control',
      description: 'Inventory management with FIFO/LIFO/Average costing',
      icon: CubeIcon,
      href: '/stock',
      color: 'bg-purple-500',
      stats: [
        { label: 'Stock Items', value: dashboardStats?.stock.totalItems.toString() || '0' },
        { label: 'Total Value', value: dashboardStats ? `$${dashboardStats.stock.totalValue.toLocaleString()}` : '$0' }
      ]
    },
    {
      title: 'General Ledger',
      description: 'Chart of accounts, journal entries, and financial reporting',
      icon: DocumentTextIcon,
      href: '/gl',
      color: 'bg-indigo-500',
      stats: [
        { label: 'GL Accounts', value: dashboardStats?.gl.accountsCount.toString() || '0' },
        { label: 'Period Balance', value: dashboardStats?.gl.isBalanced ? 'Balanced' : 'Unbalanced' }
      ]
    },
    {
      title: 'Financial Reports',
      description: 'Trial balance, P&L, balance sheet, and management reports',
      icon: ChartBarIcon,
      href: '/reports',
      color: 'bg-yellow-500',
      stats: [
        { label: 'Available Reports', value: dashboardStats ? `${dashboardStats.reports.availableReports}+` : '0' },
        { label: 'Last Generated', value: dashboardStats?.reports.lastGenerated || 'Never' }
      ]
    },
    {
      title: 'Payments',
      description: 'Customer receipts and supplier payment processing',
      icon: BanknotesIcon,
      href: '/payments',
      color: 'bg-emerald-500',
      stats: [
        { label: 'Pending Payments', value: dashboardStats?.payments.pendingCount.toString() || '0' },
        { label: 'Bank Balance', value: dashboardStats ? `$${dashboardStats.payments.bankBalance.toLocaleString()}` : '$0' }
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
            Welcome to ACAS
          </h2>
          <p className="text-gray-600">
            Complete ERP system providing comprehensive business management including accounting, inventory, 
            customer relations, and financial reporting.
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

      </main>
    </div>
  )
}