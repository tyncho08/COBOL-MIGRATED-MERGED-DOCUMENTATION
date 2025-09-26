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
  totalReceipts: number
  receiptsChange: number
  totalPayments: number
  paymentsChange: number
  pendingReceipts: number
  pendingPayments: number
  bankBalance: number
  netCashFlow: number
}

interface RecentTransaction {
  id: string
  type: 'receipt' | 'payment'
  reference: string
  description: string
  amount: number
  date: string
  account: string
  status: 'completed' | 'pending' | 'failed'
  category: string
}

export default function PaymentsPage() {
  const router = useRouter()
  const [summary, setSummary] = useState<PaymentSummary | null>(null)
  const [recentTransactions, setRecentTransactions] = useState<RecentTransaction[]>([])
  const [loading, setLoading] = useState(true)
  const [bankAccounts, setBankAccounts] = useState<any[]>([])
  
  // Modal states
  const [showReceiptModal, setShowReceiptModal] = useState(false)
  const [showPaymentModal, setShowPaymentModal] = useState(false)
  const [showReconciliationModal, setShowReconciliationModal] = useState(false)
  const [showAllocationModal, setShowAllocationModal] = useState(false)
  const [showImportModal, setShowImportModal] = useState(false)
  
  // Form states
  const [receiptForm, setReceiptForm] = useState({
    customerCode: '',
    amount: 0,
    paymentMethod: 'bank_transfer',
    reference: '',
    notes: ''
  })
  const [paymentForm, setPaymentForm] = useState({
    supplierCode: '',
    amount: 0,
    paymentMethod: 'bank_transfer',
    reference: '',
    notes: ''
  })

  useEffect(() => {
    const fetchPaymentsData = async () => {
      try {
        // Fetch summary data
        const summaryResponse = await fetch('http://localhost:8000/api/v1/payments/summary')
        if (summaryResponse.ok) {
          const summaryData = await summaryResponse.json()
          setSummary(summaryData.summary)
          setBankAccounts(summaryData.bankAccounts || [])
        }
        
        // Fetch recent transactions
        const transResponse = await fetch('http://localhost:8000/api/v1/payments/transactions')
        if (transResponse.ok) {
          const transData = await transResponse.json()
          setRecentTransactions(transData.transactions || [])
        }
      } catch (error) {
        console.error('Failed to fetch payments data:', error)
      } finally {
        setLoading(false)
      }
    }
    
    fetchPaymentsData()
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
          <div className="text-sm text-gray-500">{row.description}</div>
        </div>
      )
    },
    {
      key: 'amount',
      header: 'Amount',
      render: (value: any, row: RecentTransaction) => (
        <div className={`font-medium ${row.type === 'receipt' ? 'text-green-600' : 'text-red-600'}`}>
          {row.type === 'receipt' ? '+' : '-'}{formatCurrency(Math.abs(row.amount))}
        </div>
      )
    },
    {
      key: 'date',
      header: 'Date',
      render: (value: any, row: RecentTransaction) => formatDate(row.date)
    },
    {
      key: 'account',
      header: 'Account',
      render: (value: any, row: RecentTransaction) => row.account
    },
    {
      key: 'status',
      header: 'Status',
      render: (value: any, row: RecentTransaction) => getStatusBadge(row.status)
    },
    {
      key: 'actions',
      header: 'Actions',
      render: (value: any, row: RecentTransaction) => (
        <Button variant="outline" size="sm">View</Button>
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
              title="Total Receipts"
              value={formatCurrency(summary.totalReceipts)}
              icon={<ArrowDownTrayIcon className="h-6 w-6" />}
              change={{ 
                value: `${summary.receiptsChange > 0 ? '+' : ''}${summary.receiptsChange.toFixed(1)}%`, 
                type: summary.receiptsChange > 0 ? 'increase' : 'decrease' 
              }}
            />
            <StatsCard
              title="Total Payments"
              value={formatCurrency(summary.totalPayments)}
              icon={<ArrowUpTrayIcon className="h-6 w-6" />}
              change={{ 
                value: `${summary.paymentsChange > 0 ? '+' : ''}${summary.paymentsChange.toFixed(1)}%`, 
                type: summary.paymentsChange > 0 ? 'increase' : 'decrease' 
              }}
            />
            <StatsCard
              title="Pending Receipts"
              value={formatCurrency(summary.pendingReceipts)}
              icon={<ClockIcon className="h-6 w-6" />}
              change={{ 
                value: 'Outstanding from customers', 
                type: 'neutral' 
              }}
            />
            <StatsCard
              title="Bank Balance"
              value={formatCurrency(summary.bankBalance)}
              icon={<BanknotesIcon className="h-6 w-6" />}
              change={{ 
                value: `Net: ${formatCurrency(summary.netCashFlow)}`, 
                type: summary.netCashFlow > 0 ? 'increase' : 'decrease' 
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
                {bankAccounts.length === 0 ? (
                  <p className="text-gray-500 text-sm">No bank accounts configured</p>
                ) : (
                  <>
                    {bankAccounts.map((account) => (
                      <div key={account.id} className="flex justify-between items-center">
                        <div>
                          <p className="font-medium text-gray-900">{account.name}</p>
                          <p className="text-sm text-gray-500">{account.accountNumber}</p>
                        </div>
                        <p className="font-bold text-gray-900">{formatCurrency(account.balance)}</p>
                      </div>
                    ))}
                    <div className="pt-4 border-t border-gray-200">
                      <div className="flex justify-between">
                        <p className="font-bold text-gray-900">Total Balance:</p>
                        <p className="font-bold text-indigo-600">
                          {formatCurrency(bankAccounts.reduce((sum, acc) => sum + acc.balance, 0))}
                        </p>
                      </div>
                    </div>
                  </>
                )}
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
              onClick={async () => {
                try {
                  const response = await fetch('http://localhost:8000/api/v1/payments/receipt', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({
                      customer_code: receiptForm.customerCode,
                      amount: parseFloat(receiptForm.amount.toString()),
                      reference: receiptForm.reference,
                      payment_method: receiptForm.paymentMethod,
                      notes: receiptForm.notes
                    })
                  })
                  const data = await response.json()
                  if (data.success) {
                    alert(data.message || 'Receipt recorded successfully!')
                    setShowReceiptModal(false)
                    setReceiptForm({
                      customerCode: '',
                      amount: 0,
                      paymentMethod: 'bank_transfer',
                      reference: '',
                      notes: ''
                    })
                    // Refresh data
                    window.location.reload()
                  } else {
                    alert(data.message || 'Failed to record receipt')
                  }
                } catch (error) {
                  console.error('Error submitting receipt:', error)
                  alert('Failed to record receipt')
                }
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
            value={receiptForm.customerCode}
            onChange={(e) => setReceiptForm({...receiptForm, customerCode: e.target.value})}
            placeholder="Select customer"
            required
          />
          <Input
            label="Amount"
            type="number"
            value={receiptForm.amount}
            onChange={(e) => setReceiptForm({...receiptForm, amount: parseFloat(e.target.value) || 0})}
            placeholder="0.00"
            required
          />
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Payment Method
            </label>
            <select 
              className="form-select block w-full rounded-md border-gray-300 shadow-sm"
              value={receiptForm.paymentMethod}
              onChange={(e) => setReceiptForm({...receiptForm, paymentMethod: e.target.value})}
            >
              <option value="bank_transfer">Bank Transfer</option>
              <option value="check">Check</option>
              <option value="credit_card">Credit Card</option>
              <option value="cash">Cash</option>
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
              onClick={async () => {
                try {
                  const response = await fetch('http://localhost:8000/api/v1/payments/payment', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({
                      supplier_code: paymentForm.supplierCode,
                      amount: parseFloat(paymentForm.amount.toString()),
                      reference: paymentForm.reference,
                      payment_method: paymentForm.paymentMethod,
                      notes: paymentForm.notes
                    })
                  })
                  const data = await response.json()
                  if (data.success) {
                    alert(data.message || 'Payment recorded successfully!')
                    setShowPaymentModal(false)
                    setPaymentForm({
                      supplierCode: '',
                      amount: 0,
                      paymentMethod: 'bank_transfer',
                      reference: '',
                      notes: ''
                    })
                    // Refresh data
                    window.location.reload()
                  } else {
                    alert(data.message || 'Failed to record payment')
                  }
                } catch (error) {
                  console.error('Error submitting payment:', error)
                  alert('Failed to record payment')
                }
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
            value={paymentForm.supplierCode}
            onChange={(e) => setPaymentForm({...paymentForm, supplierCode: e.target.value})}
            placeholder="Select supplier"
            required
          />
          <Input
            label="Amount"
            type="number"
            value={paymentForm.amount}
            onChange={(e) => setPaymentForm({...paymentForm, amount: parseFloat(e.target.value) || 0})}
            placeholder="0.00"
            required
          />
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Payment Method
            </label>
            <select 
              className="form-select block w-full rounded-md border-gray-300 shadow-sm"
              value={paymentForm.paymentMethod}
              onChange={(e) => setPaymentForm({...paymentForm, paymentMethod: e.target.value})}
            >
              <option value="bank_transfer">Bank Transfer</option>
              <option value="check">Check</option>
              <option value="wire_transfer">Wire Transfer</option>
              <option value="ach">ACH</option>
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
            label="Notes"
            type="text"
            value={paymentForm.notes}
            onChange={(e) => setPaymentForm({...paymentForm, notes: e.target.value})}
            placeholder="Additional notes (optional)"
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