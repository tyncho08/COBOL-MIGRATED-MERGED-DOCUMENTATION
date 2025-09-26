'use client'

import { useState, useEffect } from 'react'
import { useRouter } from 'next/navigation'
import Modal from '@/components/UI/Modal'
import Input from '@/components/UI/Input'
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
import { formatCurrency, formatDate } from '@/lib/utils'

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
  const router = useRouter()
  const [summary, setSummary] = useState<PaymentSummary | null>(null)
  const [recentTransactions, setRecentTransactions] = useState<RecentTransaction[]>([])
  const [loading, setLoading] = useState(true)
  
  // Modal states
  const [showReceiptModal, setShowReceiptModal] = useState(false)
  const [showPaymentModal, setShowPaymentModal] = useState(false)
  const [showReconciliationModal, setShowReconciliationModal] = useState(false)
  const [showAllocationModal, setShowAllocationModal] = useState(false)
  const [showImportModal, setShowImportModal] = useState(false)
  
  // Form states
  const [receiptForm, setReceiptForm] = useState({
    customer: '',
    amount: '',
    method: 'Bank Transfer',
    reference: '',
    date: new Date().toISOString().split('T')[0]
  })
  const [paymentForm, setPaymentForm] = useState({
    supplier: '',
    amount: '',
    method: 'Bank Transfer',
    reference: '',
    date: new Date().toISOString().split('T')[0]
  })

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
      <Button variant="outline" size="sm" onClick={() => setShowImportModal(true)}>
        <ArrowDownTrayIcon className="h-4 w-4" />
        Import Bank Statement
      </Button>
      <Button size="sm" onClick={() => setShowPaymentModal(true)}>
        <CreditCardIcon className="h-4 w-4" />
        New Payment
      </Button>
    </div>
  )



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
          <Button variant="outline" size="sm" onClick={() => setShowAllocationModal(true)}>Allocate</Button>
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
                  <Button variant="outline" size="sm" onClick={() => alert('Transaction history coming soon!')}>
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
                <Button variant="outline" className="w-full justify-start" onClick={() => setShowReceiptModal(true)}>
                  <CreditCardIcon className="h-4 w-4 mr-2" />
                  Record Customer Receipt
                </Button>
                <Button variant="outline" className="w-full justify-start" onClick={() => setShowPaymentModal(true)}>
                  <BanknotesIcon className="h-4 w-4 mr-2" />
                  Make Supplier Payment
                </Button>
                <Button variant="outline" className="w-full justify-start" onClick={() => setShowReconciliationModal(true)}>
                  <ArrowDownTrayIcon className="h-4 w-4 mr-2" />
                  Bank Reconciliation
                </Button>
                <Button variant="outline" className="w-full justify-start" onClick={() => setShowAllocationModal(true)}>
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
      
      {/* Customer Receipt Modal */}
      <Modal
        isOpen={showReceiptModal}
        onClose={() => setShowReceiptModal(false)}
        title="Record Customer Receipt"
        size="md"
        actions={
          <>
            <Button variant="outline" onClick={() => setShowReceiptModal(false)}>
              Cancel
            </Button>
            <Button 
              className="ml-2"
              onClick={() => {
                // Handle receipt creation
                console.log('Creating receipt:', receiptForm)
                setShowReceiptModal(false)
                // Reset form
                setReceiptForm({
                  customer: '',
                  amount: '',
                  method: 'Bank Transfer',
                  reference: '',
                  date: new Date().toISOString().split('T')[0]
                })
              }}
            >
              Record Receipt
            </Button>
          </>
        }
      >
        <div className="space-y-4">
          <Input
            label="Customer"
            type="text"
            value={receiptForm.customer}
            onChange={(e) => setReceiptForm({...receiptForm, customer: e.target.value})}
            placeholder="Select customer"
            required
          />
          <Input
            label="Amount"
            type="number"
            value={receiptForm.amount}
            onChange={(e) => setReceiptForm({...receiptForm, amount: e.target.value})}
            placeholder="0.00"
            required
          />
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Payment Method
            </label>
            <select 
              className="form-select block w-full rounded-md border-gray-300 shadow-sm"
              value={receiptForm.method}
              onChange={(e) => setReceiptForm({...receiptForm, method: e.target.value})}
            >
              <option>Bank Transfer</option>
              <option>Check</option>
              <option>Credit Card</option>
              <option>Cash</option>
            </select>
          </div>
          <Input
            label="Reference"
            type="text"
            value={receiptForm.reference}
            onChange={(e) => setReceiptForm({...receiptForm, reference: e.target.value})}
            placeholder="Payment reference"
          />
          <Input
            label="Date"
            type="date"
            value={receiptForm.date}
            onChange={(e) => setReceiptForm({...receiptForm, date: e.target.value})}
            required
          />
        </div>
      </Modal>

      {/* Supplier Payment Modal */}
      <Modal
        isOpen={showPaymentModal}
        onClose={() => setShowPaymentModal(false)}
        title="Make Supplier Payment"
        size="md"
        actions={
          <>
            <Button variant="outline" onClick={() => setShowPaymentModal(false)}>
              Cancel
            </Button>
            <Button 
              className="ml-2"
              onClick={() => {
                // Handle payment creation
                console.log('Creating payment:', paymentForm)
                setShowPaymentModal(false)
                // Reset form
                setPaymentForm({
                  supplier: '',
                  amount: '',
                  method: 'Bank Transfer',
                  reference: '',
                  date: new Date().toISOString().split('T')[0]
                })
              }}
            >
              Process Payment
            </Button>
          </>
        }
      >
        <div className="space-y-4">
          <Input
            label="Supplier"
            type="text"
            value={paymentForm.supplier}
            onChange={(e) => setPaymentForm({...paymentForm, supplier: e.target.value})}
            placeholder="Select supplier"
            required
          />
          <Input
            label="Amount"
            type="number"
            value={paymentForm.amount}
            onChange={(e) => setPaymentForm({...paymentForm, amount: e.target.value})}
            placeholder="0.00"
            required
          />
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Payment Method
            </label>
            <select 
              className="form-select block w-full rounded-md border-gray-300 shadow-sm"
              value={paymentForm.method}
              onChange={(e) => setPaymentForm({...paymentForm, method: e.target.value})}
            >
              <option>Bank Transfer</option>
              <option>Check</option>
              <option>Wire Transfer</option>
              <option>ACH</option>
            </select>
          </div>
          <Input
            label="Reference"
            type="text"
            value={paymentForm.reference}
            onChange={(e) => setPaymentForm({...paymentForm, reference: e.target.value})}
            placeholder="Payment reference"
          />
          <Input
            label="Date"
            type="date"
            value={paymentForm.date}
            onChange={(e) => setPaymentForm({...paymentForm, date: e.target.value})}
            required
          />
        </div>
      </Modal>

      {/* Bank Reconciliation Modal */}
      <Modal
        isOpen={showReconciliationModal}
        onClose={() => setShowReconciliationModal(false)}
        title="Bank Reconciliation"
        size="lg"
      >
        <div className="text-center py-8">
          <BanknotesIcon className="mx-auto h-12 w-12 text-gray-400" />
          <h3 className="mt-2 text-sm font-medium text-gray-900">Bank Reconciliation</h3>
          <p className="mt-1 text-sm text-gray-500">
            Match your bank statement with system transactions
          </p>
          <div className="mt-6">
            <Button onClick={() => {
              setShowReconciliationModal(false)
              alert('Bank reconciliation functionality coming soon!')
            }}>
              Start Reconciliation
            </Button>
          </div>
        </div>
      </Modal>

      {/* Receipt Allocation Modal */}
      <Modal
        isOpen={showAllocationModal}
        onClose={() => setShowAllocationModal(false)}
        title="Allocate Receipts"
        size="lg"
      >
        <div className="text-center py-8">
          <CurrencyDollarIcon className="mx-auto h-12 w-12 text-gray-400" />
          <h3 className="mt-2 text-sm font-medium text-gray-900">Receipt Allocation</h3>
          <p className="mt-1 text-sm text-gray-500">
            You have {summary?.unallocated_receipts || 0} unallocated receipts
          </p>
          <div className="mt-6">
            <Button onClick={() => {
              setShowAllocationModal(false)
              alert('Payment allocation functionality coming soon!')
            }}>
              Start Allocation
            </Button>
          </div>
        </div>
      </Modal>

      {/* Import Bank Statement Modal */}
      <Modal
        isOpen={showImportModal}
        onClose={() => setShowImportModal(false)}
        title="Import Bank Statement"
        size="md"
      >
        <div className="space-y-4">
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Bank Account
            </label>
            <select className="form-select block w-full rounded-md border-gray-300 shadow-sm">
              <option>Main Operating Account (****1234)</option>
              <option>Savings Account (****5678)</option>
            </select>
          </div>
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Statement File
            </label>
            <input
              type="file"
              accept=".csv,.ofx,.qfx"
              className="block w-full text-sm text-gray-500 file:mr-4 file:py-2 file:px-4 file:rounded-md file:border-0 file:text-sm file:font-semibold file:bg-indigo-50 file:text-indigo-700 hover:file:bg-indigo-100"
            />
            <p className="mt-1 text-xs text-gray-500">Supported formats: CSV, OFX, QFX</p>
          </div>
          <div className="flex justify-end space-x-2 pt-4">
            <Button variant="outline" onClick={() => setShowImportModal(false)}>
              Cancel
            </Button>
            <Button onClick={() => {
              // Handle import
              setShowImportModal(false)
            }}>
              Import Statement
            </Button>
          </div>
        </div>
      </Modal>
    </div>
  )
}