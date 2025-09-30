'use client'

import { useState, useEffect } from 'react'
import { useRouter } from 'next/navigation'
import {
  CurrencyDollarIcon,
  ArrowDownTrayIcon,
  ArrowUpTrayIcon,
  CreditCardIcon,
  BanknotesIcon,
  CheckCircleIcon,
  XCircleIcon,
  ClockIcon,
  FunnelIcon,
  PrinterIcon,
  EyeIcon
} from '@heroicons/react/24/outline'
import { Card, StatsCard } from '@/components/UI/Card'
import PageHeader from '@/components/Layout/PageHeader'
import Button from '@/components/UI/Button'
import Input from '@/components/UI/Input'
import Select from '@/components/UI/Select'
import Table from '@/components/UI/Table'
import Modal from '@/components/UI/Modal'

interface PaymentTransaction {
  id: string
  type: 'receipt' | 'payment'
  reference: string
  description: string
  amount: number
  date: string
  account: string
  status: string
  category: string
}

export default function PaymentTransactionsPage() {
  const router = useRouter()
  const [transactions, setTransactions] = useState<PaymentTransaction[]>([])
  const [loading, setLoading] = useState(true)
  const [selectedTransaction, setSelectedTransaction] = useState<PaymentTransaction | null>(null)
  const [showDetailModal, setShowDetailModal] = useState(false)
  const [filters, setFilters] = useState({
    dateFrom: '',
    dateTo: '',
    type: '',
    status: '',
    paymentMethod: '',
    search: ''
  })
  const [showFilters, setShowFilters] = useState(false)

  useEffect(() => {
    fetchTransactions()
  }, [filters])

  const fetchTransactions = async () => {
    setLoading(true)
    try {
      const queryParams = new URLSearchParams()
      Object.entries(filters).forEach(([key, value]) => {
        if (value) queryParams.append(key, value)
      })
      
      const response = await fetch(`http://localhost:8000/api/v1/payments/transactions?${queryParams}`)
      if (response.ok) {
        const data = await response.json()
        setTransactions(data.transactions || [])
      } else {
        console.error('Failed to fetch transactions:', response.status)
        setTransactions([])
      }
    } catch (error) {
      console.error('Failed to fetch transactions:', error)
      setTransactions([])
    } finally {
      setLoading(false)
    }
  }

  const getStatusColor = (status: string) => {
    switch (status) {
      case 'completed':
        return 'bg-green-100 text-green-700'
      case 'pending':
        return 'bg-yellow-100 text-yellow-700'
      case 'cancelled':
        return 'bg-gray-100 text-gray-700'
      case 'failed':
        return 'bg-red-100 text-red-700'
      default:
        return 'bg-gray-100 text-gray-700'
    }
  }

  const getTypeColor = (type: string) => {
    switch (type) {
      case 'receipt':
        return 'bg-green-100 text-green-700'
      case 'payment':
        return 'bg-red-100 text-red-700'
      default:
        return 'bg-gray-100 text-gray-700'
    }
  }

  const getTypeIcon = (type: string) => {
    switch (type) {
      case 'receipt':
        return <ArrowDownTrayIcon className="h-4 w-4" />
      case 'payment':
        return <ArrowUpTrayIcon className="h-4 w-4" />
      default:
        return <BanknotesIcon className="h-4 w-4" />
    }
  }

  const formatCurrency = (value: number) => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD'
    }).format(value)
  }

  const formatDate = (dateString: string) => {
    try {
      // Handle both date formats (ISO and YYYYMMDD)
      let date: Date
      if (dateString.includes('-')) {
        date = new Date(dateString)
      } else {
        // Parse YYYYMMDD format
        const year = dateString.substring(0, 4)
        const month = dateString.substring(4, 6)
        const day = dateString.substring(6, 8)
        date = new Date(`${year}-${month}-${day}`)
      }
      
      return date.toLocaleDateString('en-GB', {
        day: '2-digit',
        month: '2-digit',
        year: 'numeric'
      })
    } catch {
      return dateString
    }
  }

  const columns = [
    {
      key: 'reference',
      header: 'Reference',
      render: (_: any, transaction: PaymentTransaction) => (
        <button
          onClick={() => {
            setSelectedTransaction(transaction)
            setShowDetailModal(true)
          }}
          className="text-indigo-600 hover:text-indigo-900 font-medium"
        >
          {transaction.reference}
        </button>
      )
    },
    {
      key: 'type',
      header: 'Type',
      render: (_: any, transaction: PaymentTransaction) => (
        <div className="flex items-center gap-2">
          <div className={`p-1 rounded ${getTypeColor(transaction.type)}`}>
            {getTypeIcon(transaction.type)}
          </div>
          <span className={`text-xs font-medium px-2 py-1 rounded-full ${getTypeColor(transaction.type)}`}>
            {transaction.type.toUpperCase()}
          </span>
        </div>
      )
    },
    {
      key: 'date',
      header: 'Date',
      render: (_: any, transaction: PaymentTransaction) => formatDate(transaction.date)
    },
    {
      key: 'description',
      header: 'Description',
      render: (_: any, transaction: PaymentTransaction) => (
        <div>
          <p className="font-medium text-gray-900">{transaction.description}</p>
          <p className="text-sm text-gray-500">{transaction.category}</p>
        </div>
      )
    },
    {
      key: 'amount',
      header: 'Amount',
      render: (_: any, transaction: PaymentTransaction) => (
        <div className="text-right">
          <p className={`font-medium ${transaction.type === 'receipt' ? 'text-green-600' : 'text-red-600'}`}>
            {transaction.type === 'receipt' ? '+' : '-'}{formatCurrency(transaction.amount)}
          </p>
        </div>
      )
    },
    {
      key: 'account',
      header: 'Bank Account',
      render: (_: any, transaction: PaymentTransaction) => (
        <span className="text-sm">{transaction.account}</span>
      )
    },
    {
      key: 'status',
      header: 'Status',
      render: (_: any, transaction: PaymentTransaction) => (
        <span className={`text-xs font-medium px-2 py-1 rounded-full ${getStatusColor(transaction.status)}`}>
          {transaction.status.toUpperCase()}
        </span>
      )
    },
    {
      key: 'actions',
      header: 'Actions',
      render: (_: any, transaction: PaymentTransaction) => (
        <div className="flex items-center gap-2">
          <button
            onClick={() => {
              setSelectedTransaction(transaction)
              setShowDetailModal(true)
            }}
            className="p-1 text-gray-400 hover:text-gray-600"
          >
            <EyeIcon className="h-4 w-4" />
          </button>
        </div>
      )
    }
  ]

  const handleExport = (format: 'csv' | 'excel' | 'pdf') => {
    console.log(`Exporting transactions as ${format}`)
    alert(`Export to ${format.toUpperCase()} functionality would be implemented here`)
  }

  const clearFilters = () => {
    setFilters({
      dateFrom: '',
      dateTo: '',
      type: '',
      status: '',
      paymentMethod: '',
      search: ''
    })
  }

  // Calculate summary statistics
  const totalReceipts = transactions
    .filter(t => t.type === 'receipt' && t.status === 'completed')
    .reduce((sum, t) => sum + t.amount, 0)
  
  const totalPayments = transactions
    .filter(t => t.type === 'payment' && t.status === 'completed')
    .reduce((sum, t) => sum + t.amount, 0)
  
  const pendingTransactions = transactions.filter(t => t.status === 'pending').length
  const failedTransactions = transactions.filter(t => t.status === 'failed').length

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="Payment Transactions"
        description="View all payment receipts, payments and transfers"
        actions={
          <div className="flex items-center gap-3">
            <Button
              variant="outline"
              size="sm"
              onClick={() => setShowFilters(!showFilters)}
              icon={<FunnelIcon className="h-4 w-4" />}
            >
              Filters
              {Object.values(filters).some(v => v) && (
                <span className="ml-2 h-2 w-2 bg-indigo-600 rounded-full"></span>
              )}
            </Button>
            <Button
              variant="outline"
              size="sm"
              onClick={() => handleExport('excel')}
              icon={<ArrowDownTrayIcon className="h-4 w-4" />}
            >
              Export
            </Button>
            <Button
              variant="outline"
              size="sm"
              onClick={() => window.print()}
              icon={<PrinterIcon className="h-4 w-4" />}
            >
              Print
            </Button>
          </div>
        }
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'Payments', href: '/payments' },
          { label: 'Transactions' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Summary Stats */}
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
          <StatsCard
            title="Total Receipts"
            value={formatCurrency(totalReceipts)}
            icon={<ArrowDownTrayIcon className="h-6 w-6" />}
            change={{ 
              value: `${transactions.filter(t => t.type === 'receipt').length} transactions`, 
              type: 'neutral' 
            }}
          />
          <StatsCard
            title="Total Payments"
            value={formatCurrency(totalPayments)}
            icon={<ArrowUpTrayIcon className="h-6 w-6" />}
            change={{ 
              value: `${transactions.filter(t => t.type === 'payment').length} transactions`, 
              type: 'neutral' 
            }}
          />
          <StatsCard
            title="Pending"
            value={pendingTransactions.toString()}
            icon={<ClockIcon className="h-6 w-6" />}
            change={{ 
              value: 'Awaiting processing', 
              type: pendingTransactions > 0 ? 'decrease' : 'neutral' 
            }}
          />
          <StatsCard
            title="Failed"
            value={failedTransactions.toString()}
            icon={<XCircleIcon className="h-6 w-6" />}
            change={{ 
              value: 'Require attention', 
              type: failedTransactions > 0 ? 'decrease' : 'neutral' 
            }}
          />
        </div>

        {/* Filters */}
        {showFilters && (
          <Card className="mb-6">
            <div className="p-6">
              <div className="grid grid-cols-1 md:grid-cols-3 lg:grid-cols-6 gap-4">
                <Input
                  label="Date From"
                  type="date"
                  value={filters.dateFrom}
                  onChange={(e) => setFilters({ ...filters, dateFrom: e.target.value })}
                />
                <Input
                  label="Date To"
                  type="date"
                  value={filters.dateTo}
                  onChange={(e) => setFilters({ ...filters, dateTo: e.target.value })}
                />
                <Select
                  label="Type"
                  value={filters.type}
                  onChange={(e) => setFilters({ ...filters, type: e.target.value })}
                >
                  <option value="">All Types</option>
                  <option value="receipt">Receipt</option>
                  <option value="payment">Payment</option>
                </Select>
                <Select
                  label="Status"
                  value={filters.status}
                  onChange={(e) => setFilters({ ...filters, status: e.target.value })}
                >
                  <option value="">All Statuses</option>
                  <option value="completed">Completed</option>
                  <option value="pending">Pending</option>
                  <option value="cancelled">Cancelled</option>
                  <option value="failed">Failed</option>
                </Select>
                <Input
                  label="Search"
                  type="text"
                  value={filters.search}
                  onChange={(e) => setFilters({ ...filters, search: e.target.value })}
                  placeholder="Search reference..."
                />
              </div>
              <div className="mt-4 flex justify-end">
                <Button
                  variant="ghost"
                  size="sm"
                  onClick={clearFilters}
                >
                  Clear Filters
                </Button>
              </div>
            </div>
          </Card>
        )}

        {/* Transactions Table */}
        <Card>
          <div className="px-6 py-4 border-b border-gray-200">
            <h3 className="text-lg font-medium text-gray-900">Transaction History</h3>
          </div>
          <Table
            data={transactions}
            columns={columns}
            loading={loading}
            emptyMessage="No transactions found"
          />
        </Card>
      </main>

      {/* Transaction Detail Modal */}
      <Modal
        isOpen={showDetailModal}
        onClose={() => setShowDetailModal(false)}
        title="Transaction Details"
        size="lg"
      >
        {selectedTransaction && (
          <div className="space-y-6">
            {/* Transaction Header */}
            <div className="bg-gray-50 p-4 rounded-lg">
              <div className="grid grid-cols-2 gap-4">
                <div>
                  <p className="text-sm text-gray-500">Reference</p>
                  <p className="font-medium">{selectedTransaction.reference}</p>
                </div>
                <div>
                  <p className="text-sm text-gray-500">Status</p>
                  <span className={`text-xs font-medium px-2 py-1 rounded-full ${getStatusColor(selectedTransaction.status)}`}>
                    {selectedTransaction.status.toUpperCase()}
                  </span>
                </div>
                <div>
                  <p className="text-sm text-gray-500">Type</p>
                  <div className="flex items-center gap-2">
                    <span className={`text-xs font-medium px-2 py-1 rounded-full ${getTypeColor(selectedTransaction.type)}`}>
                      {selectedTransaction.type.toUpperCase()}
                    </span>
                  </div>
                </div>
                <div>
                  <p className="text-sm text-gray-500">Date</p>
                  <p className="font-medium">{formatDate(selectedTransaction.date)}</p>
                </div>
                <div>
                  <p className="text-sm text-gray-500">Description</p>
                  <p className="font-medium">{selectedTransaction.description}</p>
                </div>
                <div>
                  <p className="text-sm text-gray-500">Category</p>
                  <p className="font-medium">{selectedTransaction.category}</p>
                </div>
                <div>
                  <p className="text-sm text-gray-500">Bank Account</p>
                  <p className="font-medium">{selectedTransaction.account}</p>
                </div>
                <div>
                  <p className="text-sm text-gray-500">Amount</p>
                  <p className={`font-medium ${selectedTransaction.type === 'receipt' ? 'text-green-600' : 'text-red-600'}`}>
                    {selectedTransaction.type === 'receipt' ? '+' : '-'}{formatCurrency(selectedTransaction.amount)}
                  </p>
                </div>
              </div>
            </div>

            {/* Actions */}
            <div className="flex justify-end gap-3">
              <Button
                variant="outline"
                onClick={() => window.print()}
                icon={<PrinterIcon className="h-4 w-4" />}
              >
                Print
              </Button>
              <Button
                variant="ghost"
                onClick={() => setShowDetailModal(false)}
              >
                Close
              </Button>
            </div>
          </div>
        )}
      </Modal>
    </div>
  )
}