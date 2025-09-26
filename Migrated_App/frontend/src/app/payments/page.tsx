'use client'

import { useState, useEffect } from 'react'
import { 
  CurrencyDollarIcon,
  CreditCardIcon,
  BanknotesIcon,
  ArrowDownTrayIcon,
  ArrowUpTrayIcon,
  CheckCircleIcon,
  ClockIcon,
  XCircleIcon
} from '@heroicons/react/24/outline'
import { Card, StatsCard } from '@/components/UI/Card'
import Button from '@/components/UI/Button'
import PageHeader from '@/components/Layout/PageHeader'
import Table from '@/components/UI/Table'

interface PaymentSummary {
  total_receipts_today: number
  total_payments_today: number
  pending_receipts: number
  pending_payments: number
  unallocated_receipts: number
  bank_balance: number
}

interface RecentTransaction {
  id: number
  type: 'receipt' | 'payment'
  reference: string
  customer_supplier: string
  amount: number
  date: string
  method: string
  status: 'completed' | 'pending' | 'failed'
  allocated: boolean
}

export default function PaymentsPage() {
  const [summary, setSummary] = useState<PaymentSummary | null>(null)
  const [recentTransactions, setRecentTransactions] = useState<RecentTransaction[]>([])
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    // Simulate API calls
    setTimeout(() => {
      setSummary({
        total_receipts_today: 12450.00,
        total_payments_today: 8950.00,
        pending_receipts: 5,
        pending_payments: 3,
        unallocated_receipts: 7,
        bank_balance: 125340.50
      })

      setRecentTransactions([
        {
          id: 1,
          type: 'receipt',
          reference: 'RCT-2024-0156',
          customer_supplier: 'ABC Manufacturing Ltd',
          amount: 2450.00,
          date: '2024-02-20',
          method: 'Bank Transfer',
          status: 'completed',
          allocated: true
        },
        {
          id: 2,
          type: 'payment',
          reference: 'PAY-2024-0089',
          customer_supplier: 'Supplier XYZ Ltd',
          amount: 1850.00,
          date: '2024-02-20',
          method: 'Check',
          status: 'pending',
          allocated: false
        },
        {
          id: 3,
          type: 'receipt',
          reference: 'RCT-2024-0155',
          customer_supplier: 'Tech Solutions Inc',
          amount: 5200.00,
          date: '2024-02-19',
          method: 'Credit Card',
          status: 'completed',
          allocated: false
        }
      ])
      setLoading(false)
    }, 1000)
  }, [])

  const quickActions = (
    <div className="flex space-x-2">
      <Button variant="outline" size="sm">
        <ArrowDownTrayIcon className="h-4 w-4" />
        Import Bank Statement
      </Button>
      <Button size="sm">
        <CreditCardIcon className="h-4 w-4" />
        New Payment
      </Button>
    </div>
  )

  const formatCurrency = (amount: number) => {
    return new Intl.NumberFormat('en-GB', {
      style: 'currency',
      currency: 'GBP'
    }).format(amount)
  }

  const formatDate = (dateString: string) => {
    return new Date(dateString).toLocaleDateString('en-GB', {
      day: '2-digit',
      month: '2-digit',
      year: 'numeric'
    })
  }

  const getStatusBadge = (status: string) => {
    switch (status) {
      case 'completed':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800">
            <CheckCircleIcon className="w-3 h-3 mr-1" />
            Completed
          </span>
        )
      case 'pending':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-yellow-100 text-yellow-800">
            <ClockIcon className="w-3 h-3 mr-1" />
            Pending
          </span>
        )
      case 'failed':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800">
            <XCircleIcon className="w-3 h-3 mr-1" />
            Failed
          </span>
        )
      default:
        return null
    }
  }

  const columns = [
    {
      key: 'type',
      header: 'Type',
      render: (value: any, row: RecentTransaction) => (
        <div className="flex items-center">
          {row.type === 'receipt' ? (
            <ArrowDownTrayIcon className="h-5 w-5 text-green-600 mr-2" />
          ) : (
            <ArrowUpTrayIcon className="h-5 w-5 text-red-600 mr-2" />
          )}
          <span className="font-medium capitalize">{row.type}</span>
        </div>
      )
    },
    {
      key: 'reference',
      header: 'Reference',
      render: (value: any, row: RecentTransaction) => (
        <div>
          <div className="font-medium text-gray-900">{row.reference}</div>
          <div className="text-sm text-gray-500">{row.customer_supplier}</div>
        </div>
      )
    },
    {
      key: 'amount',
      header: 'Amount',
      render: (value: any, row: RecentTransaction) => (
        <div className={`font-medium ${row.type === 'receipt' ? 'text-green-600' : 'text-red-600'}`}>
          {row.type === 'receipt' ? '+' : '-'}{formatCurrency(row.amount)}
        </div>
      )
    },
    {
      key: 'date',
      header: 'Date',
      render: (value: any, row: RecentTransaction) => formatDate(row.date)
    },
    {
      key: 'method',
      header: 'Method',
      render: (value: any) => value
    },
    {
      key: 'status',
      header: 'Status',
      render: (value: any, row: RecentTransaction) => getStatusBadge(row.status)
    },
    {
      key: 'allocated',
      header: 'Allocated',
      render: (value: any, row: RecentTransaction) => (
        row.allocated ? (
          <CheckCircleIcon className="h-5 w-5 text-green-600" />
        ) : (
          <Button variant="outline" size="sm">Allocate</Button>
        )
      )
    }
  ]

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="Payments"
        description="Customer receipts and supplier payment processing"
        actions={quickActions}
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'Payments' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Summary Stats */}
        {summary && (
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
            <StatsCard
              title="Receipts Today"
              value={formatCurrency(summary.total_receipts_today)}
              icon={<ArrowDownTrayIcon className="h-6 w-6" />}
              change={{ 
                value: `${summary.pending_receipts} pending`, 
                type: 'neutral' 
              }}
            />
            <StatsCard
              title="Payments Today"
              value={formatCurrency(summary.total_payments_today)}
              icon={<ArrowUpTrayIcon className="h-6 w-6" />}
              change={{ 
                value: `${summary.pending_payments} pending`, 
                type: 'neutral' 
              }}
            />
            <StatsCard
              title="Unallocated"
              value={summary.unallocated_receipts.toString()}
              icon={<ClockIcon className="h-6 w-6" />}
              change={{ 
                value: 'Receipts to allocate', 
                type: 'decrease' 
              }}
            />
            <StatsCard
              title="Bank Balance"
              value={formatCurrency(summary.bank_balance)}
              icon={<BanknotesIcon className="h-6 w-6" />}
              change={{ 
                value: '+3.2% this month', 
                type: 'increase' 
              }}
            />
          </div>
        )}

        <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
          {/* Recent Transactions */}
          <div className="lg:col-span-2">
            <Card>
              <div className="px-6 py-4 border-b border-gray-200">
                <div className="flex items-center justify-between">
                  <h3 className="text-lg font-medium text-gray-900">Recent Transactions</h3>
                  <Button variant="outline" size="sm">
                    View All
                  </Button>
                </div>
              </div>
              <Table
                data={recentTransactions}
                columns={columns}
                loading={loading}
                emptyMessage="No transactions found"
              />
            </Card>
          </div>

          {/* Quick Actions & Bank Accounts */}
          <div className="space-y-6">
            {/* Quick Actions */}
            <Card>
              <div className="px-6 py-4 border-b border-gray-200">
                <h3 className="text-lg font-medium text-gray-900">Quick Actions</h3>
              </div>
              <div className="p-6 space-y-3">
                <Button variant="outline" className="w-full justify-start">
                  <CreditCardIcon className="h-4 w-4 mr-2" />
                  Record Customer Receipt
                </Button>
                <Button variant="outline" className="w-full justify-start">
                  <BanknotesIcon className="h-4 w-4 mr-2" />
                  Make Supplier Payment
                </Button>
                <Button variant="outline" className="w-full justify-start">
                  <ArrowDownTrayIcon className="h-4 w-4 mr-2" />
                  Bank Reconciliation
                </Button>
                <Button variant="outline" className="w-full justify-start">
                  <CurrencyDollarIcon className="h-4 w-4 mr-2" />
                  Allocate Receipts
                </Button>
              </div>
            </Card>

            {/* Bank Accounts */}
            <Card>
              <div className="px-6 py-4 border-b border-gray-200">
                <h3 className="text-lg font-medium text-gray-900">Bank Accounts</h3>
              </div>
              <div className="p-6 space-y-4">
                <div className="flex justify-between items-center">
                  <div>
                    <p className="font-medium text-gray-900">Main Operating Account</p>
                    <p className="text-sm text-gray-500">****1234</p>
                  </div>
                  <p className="font-bold text-gray-900">{formatCurrency(125340.50)}</p>
                </div>
                <div className="flex justify-between items-center">
                  <div>
                    <p className="font-medium text-gray-900">Savings Account</p>
                    <p className="text-sm text-gray-500">****5678</p>
                  </div>
                  <p className="font-bold text-gray-900">{formatCurrency(50000.00)}</p>
                </div>
                <div className="pt-4 border-t border-gray-200">
                  <div className="flex justify-between">
                    <p className="font-bold text-gray-900">Total Balance:</p>
                    <p className="font-bold text-indigo-600">{formatCurrency(175340.50)}</p>
                  </div>
                </div>
              </div>
            </Card>
          </div>
        </div>
      </main>
    </div>
  )
}