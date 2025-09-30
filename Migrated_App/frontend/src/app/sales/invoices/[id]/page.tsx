'use client'

import { useState, useEffect } from 'react'
import { useRouter } from 'next/navigation'
import { 
  ArrowLeftIcon,
  PrinterIcon,
  EnvelopeIcon,
  DocumentDuplicateIcon,
  TrashIcon,
  CheckCircleIcon,
  ClockIcon,
  XCircleIcon,
  PencilIcon
} from '@heroicons/react/24/outline'
import { Card } from '@/components/UI/Card'
import Button from '@/components/UI/Button'
import PageHeader from '@/components/Layout/PageHeader'
import { formatCurrency, formatDate } from '@/lib/utils'
import { EmailService } from '@/lib/emailService'

interface InvoiceDetail {
  invoice_key: number
  invoice_number: string
  invoice_customer: string
  customer_name: string
  customer_address: {
    address_line1: string
    address_line2?: string
    city: string
    state: string
    zip: string
  }
  invoice_date: string
  invoice_due_date: string
  invoice_reference: string
  invoice_total_amount: number
  invoice_tax_amount: number
  invoice_paid_amount: number
  invoice_balance: number
  invoice_status: 'D' | 'O' | 'P' | 'C'
  lines: {
    line_number: number
    item_code: string
    description: string
    quantity: number
    unit_price: number
    tax_code: string
    tax_amount: number
    line_total: number
  }[]
  payments?: {
    payment_date: string
    payment_amount: number
    payment_reference: string
  }[]
}

export default function InvoiceDetailPage({ params }: { params: { id: string } }) {
  const router = useRouter()
  const [invoice, setInvoice] = useState<InvoiceDetail | null>(null)
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    fetchInvoiceDetail()
  }, [params.id])

  const fetchInvoiceDetail = async () => {
    try {
      setLoading(true)
      const response = await fetch(`http://localhost:8000/api/v1/sl/invoices/${params.id}`, {
        headers: {
          'Authorization': 'Bearer demo-token'
        }
      })
      
      if (response.ok) {
        const data = await response.json()
        setInvoice(data)
      } else if (response.status === 404) {
        alert('Invoice not found')
        router.push('/sales/invoices')
      } else {
        console.error('Failed to fetch invoice:', response.status)
      }
    } catch (error) {
      console.error('Failed to fetch invoice:', error)
    } finally {
      setLoading(false)
    }
  }

  const handlePrint = async () => {
    try {
      const response = await fetch(`http://localhost:8000/api/v1/sl/invoices/${params.id}/print?format=PDF`, {
        headers: {
          'Authorization': 'Bearer demo-token'
        }
      })
      
      if (response.ok) {
        const data = await response.json()
        // In a real app, would handle PDF download
        alert(`PDF generated for invoice ${invoice?.invoice_number}`)
      }
    } catch (error) {
      alert('Failed to generate PDF')
    }
  }

  const handleEmail = () => {
    if (!invoice) return
    
    const customerEmail = prompt(`Enter email address for ${invoice.customer_name}:`, 'customer@example.com')
    
    if (customerEmail) {
      EmailService.sendInvoice(invoice, customerEmail)
    }
  }

  const handlePost = async () => {
    if (invoice?.invoice_status !== 'D') {
      alert('Invoice is already posted')
      return
    }

    if (!confirm('Are you sure you want to post this invoice? This action cannot be undone.')) {
      return
    }

    try {
      const response = await fetch(`http://localhost:8000/api/v1/sl/invoices/${params.id}/post`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': 'Bearer demo-token'
        },
        body: JSON.stringify({
          posting_date: new Date().toISOString().split('T')[0],
          create_gl_entries: true
        })
      })
      
      if (response.ok) {
        alert('Invoice posted successfully')
        fetchInvoiceDetail()
      } else {
        const error = await response.json()
        alert(`Failed to post invoice: ${error.detail}`)
      }
    } catch (error) {
      alert('Failed to post invoice')
    }
  }

  const handleVoid = async () => {
    if (invoice?.invoice_status !== 'P') {
      alert('Only posted invoices can be voided')
      return
    }

    const reason = prompt('Please enter a reason for voiding this invoice (min 10 characters):')
    if (!reason || reason.length < 10) {
      alert('A valid reason is required')
      return
    }

    try {
      const response = await fetch(`http://localhost:8000/api/v1/sl/invoices/${params.id}/void`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': 'Bearer demo-token'
        },
        body: JSON.stringify({
          reason: reason,
          void_date: new Date().toISOString().split('T')[0]
        })
      })
      
      if (response.ok) {
        alert('Invoice voided successfully')
        fetchInvoiceDetail()
      } else {
        const error = await response.json()
        alert(`Failed to void invoice: ${error.detail}`)
      }
    } catch (error) {
      alert('Failed to void invoice')
    }
  }

  const getStatusBadge = (status: string) => {
    switch (status) {
      case 'D':
        return (
          <span className="inline-flex items-center px-3 py-1 rounded-full text-sm font-medium bg-gray-100 text-gray-800">
            <ClockIcon className="w-4 h-4 mr-1.5" />
            Draft
          </span>
        )
      case 'O':
        return (
          <span className="inline-flex items-center px-3 py-1 rounded-full text-sm font-medium bg-yellow-100 text-yellow-800">
            <ClockIcon className="w-4 h-4 mr-1.5" />
            Open
          </span>
        )
      case 'P':
        return (
          <span className="inline-flex items-center px-3 py-1 rounded-full text-sm font-medium bg-green-100 text-green-800">
            <CheckCircleIcon className="w-4 h-4 mr-1.5" />
            Paid
          </span>
        )
      case 'C':
        return (
          <span className="inline-flex items-center px-3 py-1 rounded-full text-sm font-medium bg-red-100 text-red-800">
            <XCircleIcon className="w-4 h-4 mr-1.5" />
            Cancelled
          </span>
        )
      default:
        return null
    }
  }

  if (loading) {
    return (
      <div className="min-h-screen bg-gray-50 flex items-center justify-center">
        <div className="text-lg text-gray-600">Loading invoice...</div>
      </div>
    )
  }

  if (!invoice) {
    return (
      <div className="min-h-screen bg-gray-50 flex items-center justify-center">
        <div className="text-lg text-gray-600">Invoice not found</div>
      </div>
    )
  }

  const quickActions = (
    <div className="flex space-x-2">
      {invoice.invoice_status === 'D' && (
        <>
          <Button variant="outline" size="sm">
            <PencilIcon className="h-4 w-4" />
            Edit
          </Button>
          <Button size="sm" onClick={handlePost}>
            Post Invoice
          </Button>
        </>
      )}
      {invoice.invoice_status === 'P' && (
        <Button variant="outline" size="sm" onClick={handleVoid}>
          Void Invoice
        </Button>
      )}
      <Button variant="outline" size="sm" onClick={handlePrint}>
        <PrinterIcon className="h-4 w-4" />
        Print
      </Button>
      <Button variant="outline" size="sm" onClick={handleEmail}>
        <EnvelopeIcon className="h-4 w-4" />
        Email
      </Button>
    </div>
  )

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title={`Invoice ${invoice.invoice_number}`}
        description="Invoice details and history"
        actions={quickActions}
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'Sales', href: '/sales' },
          { label: 'Invoices', href: '/sales/invoices' },
          { label: invoice.invoice_number }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
          {/* Main Invoice Details */}
          <div className="lg:col-span-2 space-y-6">
            {/* Header Info */}
            <Card>
              <div className="p-6">
                <div className="flex justify-between items-start mb-6">
                  <div>
                    <h3 className="text-lg font-semibold text-gray-900">
                      {invoice.customer_name}
                    </h3>
                    <p className="text-sm text-gray-600">Customer: {invoice.invoice_customer}</p>
                    {invoice.customer_address && (
                      <div className="mt-2 text-sm text-gray-600">
                        <p>{invoice.customer_address.address_line1}</p>
                        {invoice.customer_address.address_line2 && (
                          <p>{invoice.customer_address.address_line2}</p>
                        )}
                        <p>
                          {invoice.customer_address.city}, {invoice.customer_address.state} {invoice.customer_address.zip}
                        </p>
                      </div>
                    )}
                  </div>
                  <div>{getStatusBadge(invoice.invoice_status)}</div>
                </div>

                <div className="grid grid-cols-2 gap-4">
                  <div>
                    <p className="text-sm text-gray-500">Invoice Date</p>
                    <p className="text-sm font-medium">{formatDate(invoice.invoice_date)}</p>
                  </div>
                  <div>
                    <p className="text-sm text-gray-500">Due Date</p>
                    <p className="text-sm font-medium">{formatDate(invoice.invoice_due_date)}</p>
                  </div>
                  <div>
                    <p className="text-sm text-gray-500">Reference</p>
                    <p className="text-sm font-medium">{invoice.invoice_reference || 'N/A'}</p>
                  </div>
                  <div>
                    <p className="text-sm text-gray-500">Balance Due</p>
                    <p className="text-sm font-medium text-red-600">
                      {formatCurrency(invoice.invoice_balance)}
                    </p>
                  </div>
                </div>
              </div>
            </Card>

            {/* Line Items */}
            <Card>
              <div className="p-6">
                <h3 className="text-lg font-medium text-gray-900 mb-4">Line Items</h3>
                <div className="overflow-x-auto">
                  <table className="min-w-full divide-y divide-gray-200">
                    <thead>
                      <tr>
                        <th className="px-3 py-3 text-left text-xs font-medium text-gray-500 uppercase">
                          Description
                        </th>
                        <th className="px-3 py-3 text-right text-xs font-medium text-gray-500 uppercase">
                          Qty
                        </th>
                        <th className="px-3 py-3 text-right text-xs font-medium text-gray-500 uppercase">
                          Unit Price
                        </th>
                        <th className="px-3 py-3 text-right text-xs font-medium text-gray-500 uppercase">
                          Tax
                        </th>
                        <th className="px-3 py-3 text-right text-xs font-medium text-gray-500 uppercase">
                          Total
                        </th>
                      </tr>
                    </thead>
                    <tbody className="divide-y divide-gray-200">
                      {invoice.lines.map((line) => (
                        <tr key={line.line_number}>
                          <td className="px-3 py-4 text-sm text-gray-900">
                            {line.description}
                            {line.item_code && (
                              <span className="block text-xs text-gray-500">
                                Code: {line.item_code}
                              </span>
                            )}
                          </td>
                          <td className="px-3 py-4 text-sm text-gray-900 text-right">
                            {line.quantity}
                          </td>
                          <td className="px-3 py-4 text-sm text-gray-900 text-right">
                            {formatCurrency(line.unit_price)}
                          </td>
                          <td className="px-3 py-4 text-sm text-gray-900 text-right">
                            {formatCurrency(line.tax_amount)}
                            <span className="block text-xs text-gray-500">
                              {line.tax_code}
                            </span>
                          </td>
                          <td className="px-3 py-4 text-sm text-gray-900 text-right">
                            {formatCurrency(line.line_total)}
                          </td>
                        </tr>
                      ))}
                    </tbody>
                    <tfoot className="bg-gray-50">
                      <tr>
                        <td colSpan={4} className="px-3 py-3 text-right text-sm font-medium text-gray-900">
                          Subtotal:
                        </td>
                        <td className="px-3 py-3 text-right text-sm font-medium text-gray-900">
                          {formatCurrency(invoice.invoice_total_amount - invoice.invoice_tax_amount)}
                        </td>
                      </tr>
                      <tr>
                        <td colSpan={4} className="px-3 py-3 text-right text-sm font-medium text-gray-900">
                          Tax:
                        </td>
                        <td className="px-3 py-3 text-right text-sm font-medium text-gray-900">
                          {formatCurrency(invoice.invoice_tax_amount)}
                        </td>
                      </tr>
                      <tr>
                        <td colSpan={4} className="px-3 py-3 text-right text-sm font-bold text-gray-900">
                          Total:
                        </td>
                        <td className="px-3 py-3 text-right text-sm font-bold text-gray-900">
                          {formatCurrency(invoice.invoice_total_amount)}
                        </td>
                      </tr>
                    </tfoot>
                  </table>
                </div>
              </div>
            </Card>
          </div>

          {/* Sidebar */}
          <div className="space-y-6">
            {/* Payment Summary */}
            <Card>
              <div className="p-6">
                <h3 className="text-lg font-medium text-gray-900 mb-4">Payment Summary</h3>
                <div className="space-y-3">
                  <div className="flex justify-between">
                    <span className="text-sm text-gray-500">Invoice Total</span>
                    <span className="text-sm font-medium">
                      {formatCurrency(invoice.invoice_total_amount)}
                    </span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-sm text-gray-500">Amount Paid</span>
                    <span className="text-sm font-medium text-green-600">
                      {formatCurrency(invoice.invoice_paid_amount)}
                    </span>
                  </div>
                  <div className="pt-3 border-t">
                    <div className="flex justify-between">
                      <span className="text-sm font-medium text-gray-900">Balance Due</span>
                      <span className="text-sm font-bold text-red-600">
                        {formatCurrency(invoice.invoice_balance)}
                      </span>
                    </div>
                  </div>
                </div>
              </div>
            </Card>

            {/* Payment History */}
            {invoice.payments && invoice.payments.length > 0 && (
              <Card>
                <div className="p-6">
                  <h3 className="text-lg font-medium text-gray-900 mb-4">Payment History</h3>
                  <div className="space-y-3">
                    {invoice.payments.map((payment, index) => (
                      <div key={index} className="border-b pb-3 last:border-0">
                        <div className="flex justify-between items-start">
                          <div>
                            <p className="text-sm font-medium text-gray-900">
                              {formatCurrency(payment.payment_amount)}
                            </p>
                            <p className="text-xs text-gray-500">
                              {formatDate(payment.payment_date)}
                            </p>
                          </div>
                          <p className="text-xs text-gray-600">
                            {payment.payment_reference}
                          </p>
                        </div>
                      </div>
                    ))}
                  </div>
                </div>
              </Card>
            )}

            {/* Actions */}
            <Card>
              <div className="p-6">
                <h3 className="text-lg font-medium text-gray-900 mb-4">Actions</h3>
                <div className="space-y-2">
                  <Button
                    className="w-full justify-center"
                    variant="outline"
                    onClick={() => router.push('/sales/invoices')}
                  >
                    <ArrowLeftIcon className="h-4 w-4 mr-2" />
                    Back to Invoices
                  </Button>
                  <Button
                    className="w-full justify-center"
                    variant="outline"
                  >
                    <DocumentDuplicateIcon className="h-4 w-4 mr-2" />
                    Duplicate Invoice
                  </Button>
                  {invoice.invoice_status === 'O' && invoice.invoice_balance > 0 && (
                    <Button
                      className="w-full justify-center"
                      onClick={() => router.push('/payments?invoice=' + invoice.invoice_number)}
                    >
                      Record Payment
                    </Button>
                  )}
                </div>
              </div>
            </Card>
          </div>
        </div>
      </main>
    </div>
  )
}