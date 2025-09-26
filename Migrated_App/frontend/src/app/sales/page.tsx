'use client'

import { useState, useEffect } from 'react'
import { useRouter } from 'next/navigation'
import Modal from '@/components/UI/Modal'
import Input from '@/components/UI/Input'
import { 
  UsersIcon,
  DocumentTextIcon,
  CurrencyDollarIcon,
  ClockIcon,
  ExclamationTriangleIcon,
  ChartBarIcon,
  CreditCardIcon
} from '@heroicons/react/24/outline'
import { Card, StatsCard } from '@/components/UI/Card'
import Button from '@/components/UI/Button'
import PageHeader from '@/components/Layout/PageHeader'
import { formatCurrency, formatDate } from '@/lib/utils'

interface SLSummary {
  total_customers: number
  active_customers: number
  total_outstanding: number
  overdue_amount: number
  current_month_sales: number
  invoices_pending: number
  credit_notes_pending: number
  average_payment_days: number
}

interface RecentInvoice {
  id: number
  invoice_number: string
  customer_code: string
  customer_name: string
  amount: number
  due_date: string
  status: 'paid' | 'outstanding' | 'overdue'
  days_outstanding?: number
}

interface AgingBucket {
  period: string
  amount: number
  count: number
}

export default function SalesLedgerPage() {
  const router = useRouter()
  const [summary, setSummary] = useState<SLSummary | null>(null)
  const [recentInvoices, setRecentInvoices] = useState<RecentInvoice[]>([])
  const [agingData, setAgingData] = useState<AgingBucket[]>([])
  const [loading, setLoading] = useState(true)
  
  // Modal states
  const [showInvoiceModal, setShowInvoiceModal] = useState(false)
  const [showPaymentModal, setShowPaymentModal] = useState(false)
  const [showCustomerModal, setShowCustomerModal] = useState(false)
  const [showInquiryModal, setShowInquiryModal] = useState(false)
  const [showStatementModal, setShowStatementModal] = useState(false)
  const [showCreditControlModal, setShowCreditControlModal] = useState(false)
  
  // Form states
  const [invoiceForm, setInvoiceForm] = useState({
    customer: '',
    date: new Date().toISOString().split('T')[0],
    dueDate: '',
    items: [{ description: '', quantity: '', unitPrice: '', total: '' }]
  })
  const [paymentForm, setPaymentForm] = useState({
    customer: '',
    amount: '',
    reference: '',
    date: new Date().toISOString().split('T')[0]
  })

  useEffect(() => {
    const fetchData = async () => {
      try {
        // Simulate API calls
        await new Promise(resolve => setTimeout(resolve, 1000))
        
        setSummary({
          total_customers: 156,
          active_customers: 142,
          total_outstanding: 45230.50,
          overdue_amount: 8945.00,
          current_month_sales: 78450.00,
          invoices_pending: 23,
          credit_notes_pending: 3,
          average_payment_days: 32
        })

        setRecentInvoices([
          {
            id: 1,
            invoice_number: 'INV-2024-0156',
            customer_code: 'CUST001',
            customer_name: 'ABC Manufacturing Ltd',
            amount: 2450.00,
            due_date: '2024-02-15',
            status: 'outstanding',
            days_outstanding: 5
          },
          {
            id: 2,
            invoice_number: 'INV-2024-0155',
            customer_code: 'CUST002',
            customer_name: 'XYZ Services Corp',
            amount: 1850.75,
            due_date: '2024-01-30',
            status: 'overdue',
            days_outstanding: 18
          },
          {
            id: 3,
            invoice_number: 'INV-2024-0154',
            customer_code: 'CUST003',
            customer_name: 'Tech Solutions Inc',
            amount: 3200.00,
            due_date: '2024-02-10',
            status: 'paid'
          }
        ])

        setAgingData([
          { period: 'Current', amount: 15450.00, count: 45 },
          { period: '1-30 days', amount: 12680.50, count: 32 },
          { period: '31-60 days', count: 18, amount: 8945.00 },
          { period: '61-90 days', amount: 5850.00, count: 12 },
          { period: '90+ days', amount: 2305.00, count: 8 }
        ])
      } catch (error) {
        console.error('Failed to fetch sales ledger data:', error)
      } finally {
        setLoading(false)
      }
    }

    fetchData()
  }, [])

  const quickActions = (
    <div className="flex space-x-2">
      <Button variant="outline" size="sm" onClick={() => setShowInvoiceModal(true)}>
        <DocumentTextIcon className="h-4 w-4" />
        New Invoice
      </Button>
      <Button variant="outline" size="sm" onClick={() => setShowPaymentModal(true)}>
        <CreditCardIcon className="h-4 w-4" />
        Record Payment
      </Button>
      <Button size="sm" onClick={() => setShowCustomerModal(true)}>
        <UsersIcon className="h-4 w-4" />
        New Customer
      </Button>
    </div>
  )



  const getStatusColor = (status: string) => {
    switch (status) {
      case 'paid':
        return 'bg-green-100 text-green-800'
      case 'outstanding':
        return 'bg-blue-100 text-blue-800'
      case 'overdue':
        return 'bg-red-100 text-red-800'
      default:
        return 'bg-gray-100 text-gray-800'
    }
  }

  const isDueSoon = (dueDate: string) => {
    const due = new Date(dueDate)
    const today = new Date()
    const diffDays = Math.ceil((due.getTime() - today.getTime()) / (1000 * 60 * 60 * 24))
    return diffDays <= 7 && diffDays >= 0
  }

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="Sales Ledger"
        description="Customer management, invoicing, and accounts receivable"
        actions={quickActions}
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'Sales Ledger' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Summary Stats */}
        {summary && (
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
            <StatsCard
              title="Active Customers"
              value={summary.active_customers.toLocaleString()}
              icon={<UsersIcon className="h-6 w-6" />}
              change={{ 
                value: `${summary.total_customers} total`, 
                type: 'neutral' 
              }}
              href="/sales/customers"
            />
            <StatsCard
              title="Outstanding Amount"
              value={formatCurrency(summary.total_outstanding)}
              icon={<CurrencyDollarIcon className="h-6 w-6" />}
              change={{ 
                value: formatCurrency(summary.overdue_amount) + ' overdue', 
                type: summary.overdue_amount > 0 ? 'decrease' : 'neutral' 
              }}
              href="/sales/outstanding"
            />
            <StatsCard
              title="This Month Sales"
              value={formatCurrency(summary.current_month_sales)}
              icon={<ChartBarIcon className="h-6 w-6" />}
              href="/sales/reports"
            />
            <StatsCard
              title="Avg Payment Days"
              value={summary.average_payment_days}
              icon={<ClockIcon className="h-6 w-6" />}
              change={{ 
                value: `${summary.invoices_pending} pending`, 
                type: summary.invoices_pending > 0 ? 'decrease' : 'neutral' 
              }}
              href="/sales/analytics"
            />
          </div>
        )}

        <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
          {/* Recent Invoices */}
          <div className="lg:col-span-2">
            <Card>
              <div className="px-6 py-4 border-b border-gray-200">
                <div className="flex items-center justify-between">
                  <h3 className="text-lg font-medium text-gray-900">Recent Invoices</h3>
                  <Button variant="outline" size="sm" onClick={() => setShowInvoiceModal(true)}>
                    View All
                  </Button>
                </div>
              </div>
              <div className="p-0">
                {loading ? (
                  <div className="p-6">
                    <div className="animate-pulse space-y-4">
                      {Array.from({ length: 3 }).map((_, index) => (
                        <div key={index} className="flex items-center space-x-4">
                          <div className="h-10 w-10 bg-gray-300 rounded"></div>
                          <div className="flex-1 space-y-2">
                            <div className="h-4 bg-gray-300 rounded w-3/4"></div>
                            <div className="h-3 bg-gray-300 rounded w-1/2"></div>
                          </div>
                        </div>
                      ))}
                    </div>
                  </div>
                ) : (
                  <div className="divide-y divide-gray-200">
                    {recentInvoices.map((invoice) => (
                      <div key={invoice.id} className="p-6 hover:bg-gray-50">
                        <div className="flex items-center justify-between">
                          <div className="flex items-center space-x-3">
                            <div className={`px-2 py-1 text-xs font-medium rounded-full ${getStatusColor(invoice.status)}`}>
                              {invoice.status.toUpperCase()}
                            </div>
                            <div>
                              <p className="text-sm font-medium text-gray-900">
                                {invoice.invoice_number}
                              </p>
                              <p className="text-sm text-gray-500">
                                {invoice.customer_code} - {invoice.customer_name}
                              </p>
                              <div className="flex items-center space-x-2 text-xs text-gray-400">
                                <span>Due: {formatDate(invoice.due_date)}</span>
                                {invoice.days_outstanding && (
                                  <span className={`${
                                    invoice.status === 'overdue' ? 'text-red-600' : 
                                    isDueSoon(invoice.due_date) ? 'text-yellow-600' : 'text-gray-400'
                                  }`}>
                                    ({invoice.days_outstanding} days)
                                  </span>
                                )}
                              </div>
                            </div>
                          </div>
                          <div className="text-right">
                            <p className="text-sm font-medium text-gray-900">
                              {formatCurrency(invoice.amount)}
                            </p>
                            {isDueSoon(invoice.due_date) && invoice.status === 'outstanding' && (
                              <p className="text-xs text-yellow-600 font-medium">
                                Due Soon
                              </p>
                            )}
                          </div>
                        </div>
                      </div>
                    ))}
                  </div>
                )}
              </div>
            </Card>
          </div>

          {/* Aging Analysis & Actions */}
          <div className="space-y-6">
            {/* Aging Analysis */}
            <Card>
              <div className="px-6 py-4 border-b border-gray-200">
                <h3 className="text-lg font-medium text-gray-900">Aging Analysis</h3>
              </div>
              <div className="p-6">
                {loading ? (
                  <div className="animate-pulse space-y-3">
                    {Array.from({ length: 5 }).map((_, index) => (
                      <div key={index} className="flex justify-between">
                        <div className="h-3 bg-gray-300 rounded w-1/2"></div>
                        <div className="h-3 bg-gray-300 rounded w-1/4"></div>
                      </div>
                    ))}
                  </div>
                ) : (
                  <div className="space-y-3">
                    {agingData.map((bucket) => (
                      <div key={bucket.period} className="flex justify-between items-center">
                        <div>
                          <p className="text-sm font-medium text-gray-900">{bucket.period}</p>
                          <p className="text-xs text-gray-500">{bucket.count} invoices</p>
                        </div>
                        <div className="text-right">
                          <p className="text-sm font-bold text-gray-900">
                            {formatCurrency(bucket.amount)}
                          </p>
                        </div>
                      </div>
                    ))}
                    <div className="pt-3 border-t border-gray-200">
                      <div className="flex justify-between">
                        <span className="font-bold text-gray-900">Total Outstanding:</span>
                        <span className="font-bold text-indigo-600">
                          {formatCurrency(agingData.reduce((sum, bucket) => sum + bucket.amount, 0))}
                        </span>
                      </div>
                    </div>
                  </div>
                )}
              </div>
            </Card>

            {/* Quick Actions */}
            <Card>
              <div className="px-6 py-4 border-b border-gray-200">
                <h3 className="text-lg font-medium text-gray-900">Quick Actions</h3>
              </div>
              <div className="p-6 space-y-3">
                <Button variant="outline" className="w-full justify-start" onClick={() => setShowInquiryModal(true)}>
                  <UsersIcon className="h-4 w-4 mr-2" />
                  Customer Inquiry
                </Button>
                <Button variant="outline" className="w-full justify-start" onClick={() => setShowInvoiceModal(true)}>
                  <DocumentTextIcon className="h-4 w-4 mr-2" />
                  Create Invoice
                </Button>
                <Button variant="outline" className="w-full justify-start" onClick={() => setShowPaymentModal(true)}>
                  <CreditCardIcon className="h-4 w-4 mr-2" />
                  Record Payment
                </Button>
                <Button variant="outline" className="w-full justify-start" onClick={() => setShowStatementModal(true)}>
                  <ChartBarIcon className="h-4 w-4 mr-2" />
                  Customer Statement
                </Button>
                <Button variant="outline" className="w-full justify-start" onClick={() => setShowCreditControlModal(true)}>
                  <ExclamationTriangleIcon className="h-4 w-4 mr-2" />
                  Credit Control
                </Button>
              </div>
            </Card>

            {/* Key Metrics */}
            {summary && (
              <Card>
                <div className="px-6 py-4 border-b border-gray-200">
                  <h3 className="text-lg font-medium text-gray-900">Key Metrics</h3>
                </div>
                <div className="p-6">
                  <dl className="space-y-3 text-sm">
                    <div className="flex justify-between">
                      <dt className="text-gray-500">Collection Rate:</dt>
                      <dd className="font-medium text-gray-900">85.2%</dd>
                    </div>
                    <div className="flex justify-between">
                      <dt className="text-gray-500">DSO (Days Sales Outstanding):</dt>
                      <dd className="font-medium text-gray-900">{summary.average_payment_days} days</dd>
                    </div>
                    <div className="flex justify-between">
                      <dt className="text-gray-500">Bad Debt Provision:</dt>
                      <dd className="font-medium text-gray-900">2.1%</dd>
                    </div>
                    <div className="flex justify-between">
                      <dt className="text-gray-500">Credit Notes This Month:</dt>
                      <dd className="font-medium text-gray-900">{summary.credit_notes_pending}</dd>
                    </div>
                  </dl>
                </div>
              </Card>
            )}
          </div>
        </div>

        {/* Alerts */}
        {summary && (summary.overdue_amount > 0 || summary.invoices_pending > 0) && (
          <div className="mt-8">
            <Card>
              <div className="p-6">
                <div className="rounded-md bg-yellow-50 p-4">
                  <div className="flex">
                    <div className="flex-shrink-0">
                      <ExclamationTriangleIcon className="h-5 w-5 text-yellow-400" />
                    </div>
                    <div className="ml-3">
                      <h3 className="text-sm font-medium text-yellow-800">
                        Credit Control Alert
                      </h3>
                      <div className="mt-2 text-sm text-yellow-700">
                        <ul className="list-disc pl-5 space-y-1">
                          {summary.overdue_amount > 0 && (
                            <li>
                              {formatCurrency(summary.overdue_amount)} in overdue receivables requires attention
                            </li>
                          )}
                          {summary.invoices_pending > 0 && (
                            <li>{summary.invoices_pending} invoices awaiting approval or processing</li>
                          )}
                          <li>Consider sending payment reminders to customers with outstanding balances</li>
                        </ul>
                      </div>
                      <div className="mt-4">
                        <div className="flex space-x-2">
                          <Button size="sm" variant="outline" onClick={() => setShowCreditControlModal(true)}>
                            Run Credit Control
                          </Button>
                          <Button size="sm" variant="outline" onClick={() => setShowStatementModal(true)}>
                            Generate Statements
                          </Button>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
              </div>
            </Card>
          </div>
        )}
      </main>
      
      {/* Customer Inquiry Modal */}
      <Modal
        isOpen={showInquiryModal}
        onClose={() => setShowInquiryModal(false)}
        title="Customer Inquiry"
        size="md"
      >
        <div className="space-y-4">
          <Input
            label="Customer Code or Name"
            type="text"
            placeholder="Search for a customer"
          />
          <div className="text-center py-8">
            <UsersIcon className="mx-auto h-12 w-12 text-gray-400" />
            <p className="mt-2 text-sm text-gray-500">
              Enter a customer code or name to view details
            </p>
          </div>
        </div>
      </Modal>

      {/* New Invoice Modal */}
      <Modal
        isOpen={showInvoiceModal}
        onClose={() => setShowInvoiceModal(false)}
        title="Create New Invoice"
        size="lg"
        actions={
          <>
            <Button variant="outline" onClick={() => setShowInvoiceModal(false)}>
              Cancel
            </Button>
            <Button 
              className="ml-2"
              onClick={() => {
                console.log('Creating invoice:', invoiceForm)
                setShowInvoiceModal(false)
                // Reset form
                setInvoiceForm({
                  customer: '',
                  date: new Date().toISOString().split('T')[0],
                  dueDate: '',
                  items: [{ description: '', quantity: '', unitPrice: '', total: '' }]
                })
              }}
            >
              Create Invoice
            </Button>
          </>
        }
      >
        <div className="space-y-4">
          <Input
            label="Customer"
            type="text"
            value={invoiceForm.customer}
            onChange={(e) => setInvoiceForm({...invoiceForm, customer: e.target.value})}
            placeholder="Select customer"
            required
          />
          <div className="grid grid-cols-2 gap-4">
            <Input
              label="Invoice Date"
              type="date"
              value={invoiceForm.date}
              onChange={(e) => setInvoiceForm({...invoiceForm, date: e.target.value})}
              required
            />
            <Input
              label="Due Date"
              type="date"
              value={invoiceForm.dueDate}
              onChange={(e) => setInvoiceForm({...invoiceForm, dueDate: e.target.value})}
              required
            />
          </div>
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Invoice Items
            </label>
            <div className="border rounded-lg p-4">
              <div className="grid grid-cols-5 gap-2 mb-2 text-xs font-medium text-gray-500">
                <div className="col-span-2">Description</div>
                <div>Qty</div>
                <div>Unit Price</div>
                <div>Total</div>
              </div>
              {invoiceForm.items.map((item, index) => (
                <div key={index} className="grid grid-cols-5 gap-2 mb-2">
                  <input
                    type="text"
                    className="col-span-2 form-input text-sm"
                    placeholder="Item description"
                    value={item.description}
                    onChange={(e) => {
                      const items = [...invoiceForm.items]
                      items[index].description = e.target.value
                      setInvoiceForm({...invoiceForm, items})
                    }}
                  />
                  <input
                    type="number"
                    className="form-input text-sm"
                    placeholder="1"
                    value={item.quantity}
                    onChange={(e) => {
                      const items = [...invoiceForm.items]
                      items[index].quantity = e.target.value
                      setInvoiceForm({...invoiceForm, items})
                    }}
                  />
                  <input
                    type="number"
                    className="form-input text-sm"
                    placeholder="0.00"
                    value={item.unitPrice}
                    onChange={(e) => {
                      const items = [...invoiceForm.items]
                      items[index].unitPrice = e.target.value
                      setInvoiceForm({...invoiceForm, items})
                    }}
                  />
                  <input
                    type="number"
                    className="form-input text-sm"
                    placeholder="0.00"
                    value={item.total}
                    readOnly
                  />
                </div>
              ))}
              <Button
                size="sm"
                variant="outline"
                onClick={() => {
                  setInvoiceForm({
                    ...invoiceForm,
                    items: [...invoiceForm.items, { description: '', quantity: '', unitPrice: '', total: '' }]
                  })
                }}
              >
                Add Item
              </Button>
            </div>
          </div>
        </div>
      </Modal>

      {/* Record Payment Modal */}
      <Modal
        isOpen={showPaymentModal}
        onClose={() => setShowPaymentModal(false)}
        title="Record Customer Payment"
        size="md"
        actions={
          <>
            <Button variant="outline" onClick={() => setShowPaymentModal(false)}>
              Cancel
            </Button>
            <Button 
              className="ml-2"
              onClick={() => {
                console.log('Recording payment:', paymentForm)
                setShowPaymentModal(false)
                // Reset form
                setPaymentForm({
                  customer: '',
                  amount: '',
                  reference: '',
                  date: new Date().toISOString().split('T')[0]
                })
              }}
            >
              Record Payment
            </Button>
          </>
        }
      >
        <div className="space-y-4">
          <Input
            label="Customer"
            type="text"
            value={paymentForm.customer}
            onChange={(e) => setPaymentForm({...paymentForm, customer: e.target.value})}
            placeholder="Select customer"
            required
          />
          <Input
            label="Payment Amount"
            type="number"
            value={paymentForm.amount}
            onChange={(e) => setPaymentForm({...paymentForm, amount: e.target.value})}
            placeholder="0.00"
            required
          />
          <Input
            label="Reference"
            type="text"
            value={paymentForm.reference}
            onChange={(e) => setPaymentForm({...paymentForm, reference: e.target.value})}
            placeholder="Payment reference"
          />
          <Input
            label="Payment Date"
            type="date"
            value={paymentForm.date}
            onChange={(e) => setPaymentForm({...paymentForm, date: e.target.value})}
            required
          />
        </div>
      </Modal>

      {/* New Customer Modal */}
      <Modal
        isOpen={showCustomerModal}
        onClose={() => setShowCustomerModal(false)}
        title="Create New Customer"
        size="md"
        actions={
          <>
            <Button variant="outline" onClick={() => setShowCustomerModal(false)}>
              Cancel
            </Button>
            <Button 
              className="ml-2"
              onClick={() => {
                console.log('Creating customer')
                setShowCustomerModal(false)
              }}
            >
              Create Customer
            </Button>
          </>
        }
      >
        <div className="space-y-4">
          <Input label="Customer Code" type="text" placeholder="AUTO-GENERATE" disabled />
          <Input label="Company Name" type="text" placeholder="Enter company name" required />
          <Input label="Contact Name" type="text" placeholder="Enter contact name" />
          <Input label="Email" type="email" placeholder="email@company.com" required />
          <Input label="Phone" type="tel" placeholder="+1 234 567 8900" />
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Credit Limit
            </label>
            <input type="number" className="form-input w-full" placeholder="0.00" />
          </div>
        </div>
      </Modal>

      {/* Customer Statement Modal */}
      <Modal
        isOpen={showStatementModal}
        onClose={() => setShowStatementModal(false)}
        title="Generate Customer Statements"
        size="md"
      >
        <div className="space-y-4">
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Statement Period
            </label>
            <select className="form-select block w-full">
              <option>Current Month</option>
              <option>Last Month</option>
              <option>Last 3 Months</option>
              <option>Custom Date Range</option>
            </select>
          </div>
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Customer Selection
            </label>
            <select className="form-select block w-full">
              <option>All Active Customers</option>
              <option>Customers with Outstanding Balance</option>
              <option>Specific Customer</option>
            </select>
          </div>
          <div className="flex justify-end space-x-2 pt-4">
            <Button variant="outline" onClick={() => setShowStatementModal(false)}>
              Cancel
            </Button>
            <Button onClick={() => {
              setShowStatementModal(false)
              router.push('/sales/statements')
            }}>
              Generate Statements
            </Button>
          </div>
        </div>
      </Modal>

      {/* Credit Control Modal */}
      <Modal
        isOpen={showCreditControlModal}
        onClose={() => setShowCreditControlModal(false)}
        title="Credit Control Actions"
        size="md"
      >
        <div className="text-center py-8">
          <ExclamationTriangleIcon className="mx-auto h-12 w-12 text-yellow-400" />
          <h3 className="mt-2 text-sm font-medium text-gray-900">Credit Control Review</h3>
          <p className="mt-1 text-sm text-gray-500">
            {summary ? `${formatCurrency(summary.overdue_amount)} in overdue receivables` : 'Review overdue accounts'}
          </p>
          <div className="mt-6 space-y-2">
            <Button onClick={() => {
              setShowCreditControlModal(false)
              router.push('/sales/credit-control')
            }}>
              Review Overdue Accounts
            </Button>
            <Button variant="outline" onClick={() => {
              setShowCreditControlModal(false)
              router.push('/sales/reminder-letters')
            }}>
              Send Reminder Letters
            </Button>
          </div>
        </div>
      </Modal>
    </div>
  )
}